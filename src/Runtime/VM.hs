{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | Hash runtime evaluation. This module contains all the necessary
-- | evaluation logic to run a Hash program.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module Runtime.VM where

import Control.Error (throwE)
import Control.Lens (element, over, view, views, (.~))
import Control.Monad.Except
  ( ExceptT (..),
    MonadTrans (lift),
    foldM,
    runExceptT,
  )
import Control.Monad.State (gets, modify)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.HashSet as HS
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Vector as V
import Runtime.Boot
import Runtime.Heap (garbageCollect, gc, groupByKeyHM, heapDeref, heapModify, newHeapVal)
import Runtime.Primitives (boolFalseV, boolTrueV)
import Runtime.Scope (addSymbol, lookupSymbol, modifySymbol, newScope, popScope)
import Runtime.Stack (addStackFrame, popStackFrame)
import Utils

runFunc :: IntrinsicFunc -> [CopyValue] -> IO CopyValue
runFunc func args =
  runSignaledExecution (lift $ func args) (emptyProgramCtx 10000)
    >>= ( \(res, _) -> case res of
            (Right r) -> return r
            _ -> return VoidV
        )

-- | Utility to allocate a string on the heap
heapAllocString :: Text -> Execution CopyValue
heapAllocString = unsignaled . evalInstruction . IStr

-- | Evaluate a tuple-like patterns
-- | First and second arguments need to have the same length.
evalTuplePattern :: V.Vector CopyValue -> [PatternR] -> SignaledExecution (Maybe [(Name, CopyValue)])
evalTuplePattern (uncons -> Nothing) [] = return . Just $ []
evalTuplePattern (uncons -> Just (v, vals)) (p : patterns) = do
  res <- evalPattern v p
  case res of
    Nothing -> return Nothing
    Just r -> evalTuplePattern vals patterns <&> (\x -> (r ++) <$> x)
evalTuplePattern _ _ = lift $ internalPanic "inconsistent evalTuplePattern arguments"

-- | Return an empty pattern result (no binds) with the given outcome.
emptyCondP :: Bool -> SignaledExecution (Maybe [(Name, CopyValue)])
emptyCondP True = return $ Just []
emptyCondP False = return Nothing

-- | Evaluate a value against a pattern.
--
-- If `Just b` is returned, the value passed the pattern test and `b` are the
-- list of name binds (could be empty). If `Nothing` is returned, the value
-- didn't pass the pattern test.
evalPattern :: CopyValue -> PatternR -> SignaledExecution (Maybe [(Name, CopyValue)])
evalPattern val (BindP name) = return . Just $ [(name, val)]
evalPattern _ IgnoreP = return . Just $ []
evalPattern (FloatV f) (FloatP p) = emptyCondP $ p == f
evalPattern (IntV i) (IntP p) = emptyCondP $ p == i
evalPattern (CharV c) (CharP p) = emptyCondP $ p == c
evalPattern (RefValue x) (StrP p) =
  lift (heapDeref x) >>= \case
    StrV s -> emptyCondP $ p == s
    _ -> lift $ internalPanic "non-string found in string pattern"
evalPattern (EnumV enumVariantV Nothing) (EnumP enumVariantP []) | enumVariantP == enumVariantV = return . Just $ []
evalPattern (EnumV enumVariantV (Just copyValsV)) (EnumP enumVariantP valsP) | enumVariantP == enumVariantV = do
  vs <- lift (heapDeref copyValsV)
  case vs of
    EnumArgs args -> evalTuplePattern args valsP
    _ -> lift $ internalPanic "Expected enum args ref in EnumV"
evalPattern (EnumV _ _) (EnumP _ _) = return Nothing
evalPattern (TupleV vals) (TupleP patterns) = evalTuplePattern vals patterns
evalPattern b (CondP pattern ins) =
  -- First evaluate the pattern
  evalPattern b pattern >>= \case
    Nothing -> return Nothing
    Just binds ->
      -- Then evaluate the condition
      evalInstruction ins >>= \case
        b | b == boolTrueV -> return . Just $ binds
        b | b == boolFalseV -> return Nothing
        _ -> lift $ internalPanic "expected boolean result of conditional"
evalPattern (RefValue rs) (StructP patterns) =
  lift (heapDeref rs) >>= \case
    StructV s -> do
      -- First evaluate the fields
      matches <- evalTuplePattern s patterns
      case matches of
        -- If no matches, return
        Nothing -> return Nothing
        -- Otherwise, add on the field binds and return.
        Just ms -> return . Just $ ms ++ extra
          where
            extra =
              mapMaybe
                ( \(p, v) -> case p of
                    BindP n -> Just (n, v)
                    _ -> Nothing
                )
                (zip patterns (V.toList s))
    _ -> lift $ internalPanic "expected struct in pattern position"
evalPattern val (OrP patterns) =
  -- Evaluate each pattern and fold on result.
  foldM
    ( \acc p -> case acc of
        Just binds -> return . Just $ binds
        _ -> evalPattern val p
    )
    Nothing
    patterns
evalPattern _ _ = lift $ internalPanic "unexpected type or variant in enum"

-- | Evaluate any combination of instruction. Essentially pattern match agaisnt
-- | all valid variants of the a instriction. During this stage if we encounter
-- | a pattern that does not match, we essentially call 'internalPanic' because we
-- | expect that the type checker only emits valid instructions.
-- |
-- | Additionally, we run within 'SingnaledExecution' in order to essentially emulate how
-- | instructions such as 'IBreak', 'IContinue' and 'IReturn' work. Since these instruction
-- | are essentially control flow statements, we need to know about what current execution
-- | context we are is so we can essentially break out of a loop instruction, or return from
-- | function. Additionally we use 'throwE' to essentially emulate signaling.
evalInstruction :: Instruction -> SignaledExecution CopyValue
evalInstruction fnDef@IFnDef {} = gc $ do
  -- Get the current scope group
  ctx <- gets . view $ stack . currFrame . scopeGroup
  -- Construct a function value
  let fnVal =
        FunctionValue
          { _fnValName = fnDefName fnDef,
            _argNames = fnDefArgNames fnDef,
            _fnInner = fnDefInner fnDef,
            _fnContext = ctx
          }
  ref <- lift $ newHeapVal (FunctionV fnVal)
  -- Return a reference to the created function
  return $ RefValue ref
evalInstruction (IMatch _ []) = gc $ return VoidV -- No cases.
evalInstruction (IMatch cond ((pat, ins) : cs)) = gc $ do
  -- Fold and evaluate cases
  res <-
    evalCase pat ins >>= \first ->
      foldM
        ( \acc (p, i) -> case acc of
            Nothing -> evalCase p i
            Just v -> return $ Just v
        )
        first
        cs
  case res of
    Just r -> return r
    Nothing -> lift $ panic "Non-exhausive patterns in match."
  where
    evalCase pat ins =
      evalInstruction cond >>= (`evalPattern` pat) >>= \case
        Nothing -> return Nothing
        Just binds -> do
          mapM_ (\(n, v) -> evalInstruction $ IBind n (ILiteral v)) binds
          Just <$> evalInstruction ins
evalInstruction loop@(ILoop inner) = gc $ do
  -- Evaluate loop body, catching any signals
  ret <- lift . runExceptT $ evalInstruction inner
  case ret of
    Left BreakS -> return VoidV -- exit on break.
    Left ContinueS -> evalInstruction loop -- continue explicitly.
    Right _ -> evalInstruction loop -- continue implicitly.
    _ -> ExceptT (return ret) -- any other signals are bubbled up.
evalInstruction fnCall@IFnCall {} = gc $ do
  -- Resolve function
  fnRef <-
    evalInstruction (fn fnCall) >>= \case
      RefValue v -> return v
      _ -> lift $ internalPanic "expected Ref from FnCall instruction"
  lift (heapDeref fnRef) >>= \case
    -- Call language function
    FunctionV fnVal -> do
      -- Add stack frame and set scope
      let newFrame =
            StackFrame
              { _callSite = fnCallSite fnCall,
                _fnName = view fnValName fnVal,
                _scopeGroup = view fnContext fnVal
              }
      lift $ addStackFrame newFrame

      -- Bind arguments
      mapM_ (evalInstruction . uncurry IBind) (zip (view argNames fnVal) (fnArgs fnCall))

      -- Call function, catching any signals
      res <- lift . runExceptT $ evalInstruction (view fnInner fnVal)

      -- Pop stack frame
      lift popStackFrame

      case res of
        Left (ReturnS ret) -> return ret
        _ -> lift $ internalPanic "expected return signal from function"
    -- Call native function
    NativeFnV (NativeFunction nativeFn) -> do
      args <- mapM evalInstruction (fnArgs fnCall)
      lift $ nativeFn args
    _ -> lift $ internalPanic "expected FunctionV or NativeFnV from FnCall deref"
evalInstruction (ILiteral v) = gc $ return v
evalInstruction m@IMap {} = gc $ do
  -- Evaluate map entries
  evalEntries <-
    mapM
      ( \(k, v) -> do
          resK <- evalInstruction k
          -- Hash the key
          hashK <-
            evalInstruction (IFnCall "<internal>" (mapHashFnRef m) [ILiteral resK]) >>= \case
              IntV h -> return $ Hashed h
              _ -> lift $ internalPanic "Expected IntV from hashing"
          resV <- evalInstruction v
          return (hashK, (resK, resV))
      )
      (mapEntries m)
  -- Construct a map with the entries
  mapRef <- lift $ newHeapVal . MapV . groupByKeyHM $ evalEntries
  return . RefValue $ mapRef
evalInstruction s@ISet {} = gc $ do
  -- Evaluate set entries
  evalEntries <-
    mapM
      ( \k -> do
          resK <- evalInstruction k
          hashK <-
            evalInstruction (IFnCall "<internal>" (setHashFnRef s) [ILiteral resK]) >>= \case
              IntV h -> return $ Hashed h
              e -> lift $ internalPanic $ "Expected IntV from hashing, got " ++ show e
          return (hashK, resK)
      )
      (setEntries s)
  -- Construct a set with the entries
  setRef <- lift $ newHeapVal . SetV . groupByKeyHM $ evalEntries
  return . RefValue $ setRef
evalInstruction (ITuple entries) = gc $ TupleV <$> V.mapM evalInstruction entries
evalInstruction (IList entries) = gc $ do
  evalEntries <- mapM evalInstruction entries
  -- Convert entries to vector
  listRef <- lift $ newHeapVal (ListV . S.fromList $ V.toList evalEntries)
  return . RefValue $ listRef
evalInstruction (IEnum variant entries) | V.length entries == 0 = gc . return $ EnumV variant Nothing
evalInstruction (IEnum variant entries) = gc $ do
  args <- V.mapM evalInstruction entries
  argsRef <- lift $ newHeapVal (EnumArgs args)
  return $ EnumV variant (Just argsRef)
evalInstruction (IStruct entries) = gc $ do
  structVal <- StructV <$> mapM evalInstruction entries
  ref <- lift $ newHeapVal structVal
  return (RefValue ref)
evalInstruction (IPatBind pat ins) = gc $ do
  evalInstruction ins >>= (`evalPattern` pat) >>= \case
    Nothing -> return VoidV
    Just binds -> do
      mapM_ (\(n, v) -> evalInstruction $ IBind n (ILiteral v)) binds
      return VoidV
evalInstruction (IBind name ins) = gc $ do
  res <- evalInstruction ins
  -- Add name to scope with the result of `ins`
  modify $ over (stack . currFrame . scopeGroup) (addSymbol name res)
  return VoidV
evalInstruction (INameRef ref) =
  gc $
    -- Access the value from the current scope group.
    gets $ views (stack . currFrame . scopeGroup) (lookupSymbol ref)
evalInstruction (IRef ref) = gc $ return (RefValue ref)
evalInstruction (IReturn inner) = gc $ do
  res <- evalInstruction inner
  throwE $ ReturnS res
evalInstruction IBreak = gc $ throwE BreakS
evalInstruction IContinue = gc $ throwE ContinueS
evalInstruction (IBlock inner) = gc $ do
  -- Create a new scope
  modify $ over (stack . currFrame . scopeGroup) newScope
  -- Run inner, catching any signals.
  res <- lift . runExceptT $ evalInstruction inner
  -- First delete scope
  modify $ over (stack . currFrame . scopeGroup) popScope
  -- Return any signals
  ExceptT (return res)
evalInstruction (ISeq xs) = gc $ foldM (\_ x -> evalInstruction x) VoidV xs
evalInstruction (IPropGet s prop) =
  gc $
    evalInstruction s >>= \case
      -- Struct
      RefValue r ->
        lift (heapDeref r) >>= \case
          StructV entries -> return $ entries V.! prop
          _ -> lift $ internalPanic "Expected struct in IPropGet"
      -- Tuple
      TupleV entries -> return $ entries V.! prop
      _ -> lift $ internalPanic "Expected ref in IPropGet"
evalInstruction (IPropSet s prop newVal) =
  gc $
    evalInstruction s >>= \case
      -- Struct (modify in place)
      RefValue r -> do
        v <- evalInstruction newVal
        lift $
          heapModify
            r
            ( \case
                StructV entries -> return $ StructV (entries & element prop .~ v)
                _ -> internalPanic "Expected struct in IPropGet"
            )
        return VoidV
      _ -> lift $ internalPanic "Expected RefValue in IPropGet"
evalInstruction (ILogicalAnd a b) = gc $ do
  -- Evaluate first clause
  resA <- evalInstruction a
  case resA of
    -- If true, evaluate second clause
    _ | resA == boolTrueV -> do
      resB <- evalInstruction b
      case resB of
        _ | resB == boolTrueV -> return boolTrueV
        _ | resB == boolFalseV -> return boolFalseV
        _ -> lift $ internalPanic "Unexpected second argument to logical and"
    _ | resA == boolFalseV -> return boolFalseV
    _ -> lift $ internalPanic "Unexpected first argument to logical and"
evalInstruction (ILogicalOr a b) = gc $ do
  -- Evaluate first clause
  resA <- evalInstruction a
  case resA of
    _ | resA == boolTrueV -> return boolTrueV
    -- If false, evaluate second clause
    _ | resA == boolFalseV -> do
      resB <- evalInstruction b
      case resB of
        _ | resB == boolTrueV -> return boolTrueV
        _ | resB == boolFalseV -> return boolFalseV
        _ -> lift $ internalPanic "Unexpected second argument to logical or"
    _ -> lift $ internalPanic "Unexpected first argument to logical or"
evalInstruction (IStr s) = gc $ RefValue <$> lift (newHeapVal (StrV s))
evalInstruction (IChar c) = gc $ return $ CharV c
evalInstruction (INameSet n ins) = gc $ do
  res <- evalInstruction ins
  modify $ over (stack . currFrame . scopeGroup) (modifySymbol n (const res))
  return VoidV
evalInstruction IGC = do
  lift $ garbageCollect HS.empty
  return VoidV
