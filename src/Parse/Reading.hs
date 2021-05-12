{-# LANGUAGE OverloadedStrings #-}

-- | Hash compiler module reading functionality. This module contains utilities
-- | and functions to read in modules, and analyse their dependencies.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module Parse.Reading where

import Control.Error (ExceptT, fromMaybe, throwE, tryIO)
import Control.Lens (over, view, (%~))
import Control.Monad.Except (MonadIO (liftIO), withExceptT)
import Control.Monad.State (StateT, gets, modify, runState)
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (catMaybes, fromJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TI
import Embed (standardLibrary)
import Error (HashError (ModuleImportError, ParsingError))
import Panic
import qualified Parse.Ast as A
import Parse.Boot
import qualified Parse.Boot as PB
import System.Environment ()
import System.FilePath (takeDirectory)
import System.IO ()
import Text.Megaparsec (runParserT)
import qualified Text.Megaparsec as M
import Utils (resolveModPath)

-- | Function to get the dependencies of a module by analysing the module graph.
addModuleDependencies :: FilePath -> HS.HashSet FilePath -> ModuleGraph -> ModuleGraph
addModuleDependencies mod toAdd graph = ModuleGraph $ HM.insertWith HS.union mod toAdd (modGraphMap graph)

-- | Function to get the dependencies of a module by analysing the module graph.
moduleDependencies :: FilePath -> ModuleGraph -> HS.HashSet FilePath
moduleDependencies mod graph = fromJust $ HM.lookup mod (modGraphMap graph)

-- | Function to initialise module cycle decection
detectModuleCycle :: FilePath -> ModuleGraph -> Maybe FilePath
detectModuleCycle = detectModuleCycle' HS.empty

-- | Function which analysed the module graph dependencies for cycles. Cycles
-- | can be detected by traversing the tree to see if it can stumble upon a module
-- | it has already seen for a given path. The algorithm is initiated every time a
-- | new dependency is found. If a cycle is found, the module name will be returned,
-- | otherwise nothing.
detectModuleCycle' :: HS.HashSet FilePath -> FilePath -> ModuleGraph -> Maybe FilePath
detectModuleCycle' seen entry graph =
  if entry `HS.member` seen
    then Just entry
    else listToMaybe . catMaybes . HS.toList $ childCycles
  where
    deps = moduleDependencies entry graph
    childCycles = HS.map (\d -> detectModuleCycle' (HS.insert entry seen) d graph) deps

-- | The empty parser context, whenever a new job is created, this context is used to
-- | parse a module.
emptyParsingCtx :: ParsingCtx
emptyParsingCtx = ParsingCtx HM.empty HS.empty (ModuleGraph HM.empty)

-- | This function is used to read in a given module, resolve it's dependencies and
-- | ensure that if it has any dependencies are not being read twice in order to reduce
-- | the ammount of work we do, we want to avoid readiing a module twice.
readInModule :: FilePath -> ExceptT HashError (StateT ParsingCtx IO) ()
readInModule moduleName = do
  contents <-
    if moduleName `HM.member` standardLibrary
      then return $ fromMaybe (internalPanicPure "Panic: Couldn't load standard library") (moduleName `HM.lookup` standardLibrary)
      else
        withExceptT
          (const (ModuleImportError $ "Couldn't import module " ++ moduleName))
          (tryIO (TI.readFile moduleName))

  -- Run the pModule parser and generate a 'ParsingCtx' by recursively calling 'readInModule' if
  -- at the end there are any 'outstanding' module imports which need to be parsed.
  case runState (runParserT (A.pModule <* M.eof) moduleName contents) (A.emptyModuleContext moduleName) of
    (Left e, _) -> throwE . ParsingError . M.errorBundlePretty $ e
    (Right mod, ModuleContext deps) -> do
      let parsedModule = ParsedModule mod moduleName moduleName contents
      modify $ over modules (HM.insert moduleName parsedModule)
      modify $ alreadyParsedPaths %~ HS.insert moduleName

      -- compute the module dependencies from the current context and prepare to parse any
      -- if there are any dependecies
      let resolvedDirname = takeDirectory moduleName
      modDeps <- liftIO $ HS.fromList <$> mapM (resolveModPath resolvedDirname) (toList deps)
      resolvedDeps <- gets . view $ alreadyParsedPaths

      -- we just parsed this module, so remove it from the diff
      let diff = HS.delete moduleName (modDeps `HS.difference` resolvedDeps)

      modify $ alreadyParsedPaths %~ (diff `HS.union`)
      modify $ moduleGraph %~ addModuleDependencies moduleName modDeps

      mapM_ readInModule diff

readREPL :: Text -> ExceptT HashError (StateT ParsingCtx IO) (PB.AstNode PB.REPLBlock)
readREPL contents = do
  let moduleName = "<interactive>"

  -- Run the pModule parser and generate a 'ParsingCtx' by recursively calling 'readInModule' if
  -- at the end there are any 'outstanding' module imports which need to be parsed.
  case runState (runParserT (A.pREPL <* M.eof) moduleName contents) (A.emptyModuleContext moduleName) of
    (Left e, _) -> throwE . ParsingError . M.errorBundlePretty $ e
    (Right mod, ModuleContext deps) -> do
      modify $ alreadyParsedPaths %~ HS.insert moduleName

      -- compute the module dependencies from the current context and prepare to parse any
      -- if there are any dependecies
      let resolvedDirname = takeDirectory moduleName
      modDeps <- liftIO $ HS.fromList <$> mapM (resolveModPath resolvedDirname) (toList deps)
      resolvedDeps <- gets . view $ alreadyParsedPaths

      -- we just parsed this module, so remove it from the diff
      let diff = HS.delete moduleName (modDeps `HS.difference` resolvedDeps)

      modify $ alreadyParsedPaths %~ (diff `HS.union`)
      modify $ moduleGraph %~ addModuleDependencies moduleName modDeps

      mapM_ readInModule diff
      return mod
