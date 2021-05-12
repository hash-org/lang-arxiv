{-# LANGUAGE TemplateHaskell #-}

-- | Hash runtime entry point. The compiler pipeline takes some arguments as files,
-- | passes this through AST, then it passes it through the typechecking stage which
-- | finally emits a Runtime tree which we finally execute.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module Pipeline where

import Control.Error (ExceptT, fromMaybe, throwE, tryIO)
import Control.Lens (makeLenses, view, views, (.~), (^.))
import Control.Monad.Except (MonadIO (liftIO), runExceptT, withExceptT)
import Control.Monad.State (StateT (runStateT), gets, modify)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import Error
import Panic
import Parse.Boot
import qualified Parse.Boot as PB
import qualified Parse.Reading as R
import Report (reportError)
import Runtime.Boot
import qualified Runtime.Boot as RB
import Runtime.Intrinsics (intrinsicMap)
import qualified Runtime.VM as RV
import qualified Typecheck.Boot as TB
import qualified Typecheck.Error as TE
import Typecheck.Traverse (getNodeType)
import Utils (resolveModPath)

data HashCtx = HashCtx
  { _parsingCtx :: ParsingCtx,
    _typeCtx :: TB.TypecheckCtx,
    _programCtx :: ProgramCtx,
    _currentPath :: FilePath
  }

makeLenses ''HashCtx

type Hash = ExceptT String (StateT HashCtx IO)

runPipeline :: Hash a -> HashCtx -> IO (Either String a, HashCtx)
runPipeline = runStateT . runExceptT

initPipeline :: FilePath -> Int -> IO HashCtx
initPipeline fp stackSize = do
  -- first we want to parse 'prelude' module, typecheck it and then return the
  -- hash context for the program to operate it with
  (pParseResE, pCtx) <- liftIO $ runStateT (runExceptT (R.readInModule "prelude")) R.emptyParsingCtx
  _ <- case pParseResE of
    Left e -> internalPanicPure $ "Prelude parsing failed: " ++ show e
    Right x -> return x

  -- we don't need to check for any module cycles and so just get the module
  let (_, parsedPrelude) = case HM.toList $ view modules pCtx of
        [] -> internalPanicPure "Failed to parse the prelude module."
        [a] -> a
        _ -> internalPanicPure "Prelude imports modules" -- This shouldn't happen.
  let pm = view moduleNode parsedPrelude

  let pTypecheckM = do
        (TB.DefinedType tId [], _, preludeI) <- getNodeType pm -- TODO: execute prelude emit
        TB.lookupTypeDef tId >>= \case
          (TB.NamespaceT _ syms) -> return (syms, TB.unwrapIns preludeI)
          _ -> internalPanicPure "Didn't get namespace from prelude"

  (preludeE, tCtx) <-
    liftIO $
      TB.runTypecheck
        pTypecheckM
        ( TB.newTypecheckCtx
            "prelude"
            TB.emptySymbolTable
            HM.empty
        )

  (preludeSyms, preludeI) <- case preludeE of
    Left (TypeError msg node) -> do
      let errorMsg = TE.constructTypeError (TE.makeError "prelude" msg (view srcText parsedPrelude) node)
      internalPanicPure $ "Prelude typechecking failed:\n" ++ errorMsg
    Right s -> return s
    _ -> internalPanicPure "Unexpected error from typechecking"

  let intrinsicHeapData =
        map
          ( \(_, (ref, fnVal)) ->
              let fnHeapVal = RB.NativeFnV (RB.NativeFunction fnVal)
               in (ref, RB.HeapEntry fnHeapVal (RB.sizeof fnHeapVal))
          )
          (HM.toList intrinsicMap)

  -- Inject all the intrinsics
  let initialCtx =
        (emptyProgramCtx stackSize)
          { RB._heap =
              RB.emptyHeap
                { _heapData = HM.fromList intrinsicHeapData,
                  _heapCounter = HM.size intrinsicMap,
                  _heapSize = sum $ map (\(_, RB.HeapEntry _ size) -> size) intrinsicHeapData
                }
          }

  liftIO $
    runSignaledExecution (RV.evalInstruction preludeI) initialCtx >>= \case
      (Right _, progCtx) -> do
        return
          HashCtx
            { _parsingCtx = pCtx,
              _typeCtx =
                tCtx
                  { TB._currentNamespace = fp,
                    TB._initialSymbols = TB.SymbolScopeGroup preludeSyms [TB.emptySymbolTable],
                    TB._symbolScopes = TB.SymbolScopeGroup preludeSyms [TB.emptySymbolTable]
                  },
              _currentPath = "",
              _programCtx = progCtx
            }
      _ -> internalPanicPure "Unexpected return from execution"

-- | This function is used to run the necessary steps it takes to run a function
-- | within the REPL. This is the process of the pipeline:
-- |
-- | - Firstly, the statement that is recievied by the pipeline is
-- | run through the Hash parser. After the statement is parsed, it is either reported
-- | that parsing failed and a 'String' representing the error is returned so that
-- | it can be displayed in the parsed. On the other hand, if parsing succeeds, we move on to
-- | typechecking.
-- |
-- | - Secondly, once an AST is generated, it is passed into the typechecker with the current
-- | typechecking context. Similarly as with the parser, either a 'String' is returned and then
-- | printed in the REPL. Alternatively, given that the typechecker immideiately begin to emit
-- | runnable instructions, it is then ready to be executed in the Hash VM.
-- |
-- | - Lastly, the instructions are executed in the VM, and then the pipeline finishes and waits
-- | for the next statement to go though the same process.
runREPLPipeline :: T.Text -> Hash ()
runREPLPipeline statement = do
  -- run the parser on the loaded module.
  (res, ctx) <- liftIO $ runStateT (runExceptT (R.readREPL statement)) R.emptyParsingCtx

  -- check here if parsing the module failed.
  case res of
    Left (ParsingError e) -> do
      liftIO $ reportError ("Failed to parse\n" ++ e) -- handle parse errors in a special way
    Left (ModuleImportError e) -> liftIO $ reportError e
    Left e -> fail . show $ e
    Right parsed -> do
      tCtx <- gets $ view typeCtx
      typecheckRes <-
        liftIO
          ( TB.runTypecheck
              (getNodeType parsed)
              tCtx
                { TB._modules = view modules ctx
                }
          )
      case typecheckRes of
        (Left (TypeError a b), _) -> do
          throwE $
            TE.constructTypeError
              ( TE.makeError
                  "<interactive>"
                  a
                  statement
                  b
              )
        (Left _, _) -> internalPanicPure "Invalid error thrown"
        (Right (_, _, ins), newTypeCtx) -> do
          modify $ typeCtx .~ newTypeCtx
          progCtx <- gets $ view programCtx
          liftIO (runSignaledExecution (RV.evalInstruction (ISeq $ maybeToList ins)) progCtx) >>= \case
            (Right _, newProgCtx) -> do
              modify $ programCtx .~ newProgCtx
            _ -> internalPanicPure "Unexpected return from execution"

-- | This function is the same as the 'runREPLPipeline', but however it runs an entire module
-- | that are just an arbitary number of statements.
runModPipeline :: FilePath -> FilePath -> [FilePath] -> Hash ()
runModPipeline workingDir moduleName _ = do
  -- resolve the module path, and prepare to run it
  resolvedFilename <-
    withExceptT
      (const ("Couldn't get module path of " ++ moduleName))
      (tryIO $ resolveModPath workingDir moduleName)

  -- run the parser on the loaded module.
  (res, ctx) <- liftIO $ runStateT (runExceptT (R.readInModule resolvedFilename)) R.emptyParsingCtx

  -- check here if parsing the module failed.
  case res of
    Left (ParsingError e) -> liftIO $ reportError ("Failed to parse\n" ++ e) -- handle parse errors in a special way
    Left (ModuleImportError e) -> liftIO $ reportError e
    Left e -> fail . show $ e
    Right _ -> do
      case R.detectModuleCycle resolvedFilename (ctx ^. moduleGraph) of
        Just f -> throwE $ "Found circular dependency in " ++ f
        _ -> do
          let modToResolve =
                fromMaybe (internalPanicPure "could not find main module") $
                  views modules (HM.lookup resolvedFilename) ctx
          tCtx <- gets $ view typeCtx
          typecheckRes <-
            liftIO
              ( TB.runTypecheck
                  (getNodeType (view moduleNode modToResolve))
                  tCtx
                    { TB._modules = view modules ctx
                    }
              )
          case typecheckRes of
            (Left (TypeError a b), _) -> do
              let filename = PB.filename b
              let src =
                    maybe
                      (internalPanicPure "could not find error module")
                      (view srcText)
                      (views modules (HM.lookup filename) ctx)
              throwE $
                TE.constructTypeError
                  ( TE.makeError
                      filename
                      a
                      src
                      b
                  )
            (Left _, _) -> internalPanicPure "Invalid error thrown"
            (Right (_, _, ins), _) -> do
              progCtx <- gets $ view programCtx
              liftIO (runSignaledExecution (RV.evalInstruction (ISeq $ maybeToList ins)) progCtx) >>= \case
                (Right _, _) -> return ()
                _ -> internalPanicPure "Unexpected return from execution"
