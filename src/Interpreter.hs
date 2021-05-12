-- | Hash interpereter definitions, including the functio to start the
-- | interactive mode and launch everything that is required for Hash
-- | to start running.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module Interpreter (start, ver) where

import Control.Error (runExceptT)
import Control.Lens (view)
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Version (showVersion)
import Panic (internalPanicPure)
import Paths_hash (version)
import Pipeline as P (Hash, runPipeline, typeCtx)
import qualified Pipeline as P
import Report (reportError)
import qualified System.Console.Haskeline as H
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode, exitSuccess)
import System.Info (os)
import System.Process (system)
import Typecheck.Boot (lookupNameByPrefix, runTypecheck)

-- | Represnting what interavtive actions can be ran within the interactivr
-- | mode.
data InteractiveAction = Clear | Quit | Path | Version deriving (Show, Eq)

-- | Utility function to nicely format the Hash interpreter vesion
ver :: String
ver = "Hash Interpreter v" ++ showVersion version

-- | Utility function to check if a specified statement is a 'InteractiveCommand'
-- | such as ':c' to clear the screen, or ':q' to quit, etc.
performAction :: InteractiveAction -> IO (Maybe ExitCode)
performAction Clear = case os of
  "mingw32" -> system "cls" >>= \x -> return $ Just x
  _ -> system "clear" >>= \x -> return $ Just x
performAction Quit = putStrLn "Goodbye!" >> exitSuccess
performAction Version = putStrLn ver >> return Nothing
performAction Path = getCurrentDirectory >>= \x -> putStrLn x >> return Nothing

-- | Commands that are prefixed with a exclamation mark, these commands are used to perform
-- | some action within the interactive mode, like quiting or clearing the current inpt
interactiveCommands :: [([Char], InteractiveAction)]
interactiveCommands =
  [ (":clear", Clear),
    (":c", Clear),
    (":quit", Quit),
    (":q", Quit),
    (":path", Path),
    (":version", Version)
  ]

-- | Function which searches for a word in the list based on what we've just typed in
lookupKeyword :: String -> Hash [H.Completion]
lookupKeyword str = do
  tCtx <- gets $ view typeCtx

  -- run the lookup and then return the results
  (query, _) <- liftIO $ runTypecheck (lookupNameByPrefix str) tCtx

  case query of
    (Left _) -> internalPanicPure "Symbol prefix lookup returned an error"
    (Right names) -> do
      return $ map H.simpleCompletion names

-- | Function to initialise Haskeline settings
-- TODO: maybe look into using Haskeline 'Alternative completion to try for keywords, repl commands?
suggestionSettings :: H.Settings Hash
suggestionSettings =
  H.Settings
    { H.historyFile = Nothing,
      H.complete = H.completeWord Nothing "\t" lookupKeyword,
      H.autoAddHistory = True
    }

-- | This is where the fun stuff happens. The interactive loop: just get a line
-- | from the standard input, parse it, typecheck it and finally execute it whilst
-- | preserving the state over each interraction. If the interaction fails, for example
-- | if the statement does not parse or typecheck, an error is printed and the
-- | 'Hash' state will be reset to the previous 'safe' state automatically.
-- | This also applies for things like calling 'panic' within the language, we essentially
-- | emulate that the VM crashes, but we don't crash vm and just trace back to the previous
-- | statement state.
repl :: H.InputT Hash ()
repl = do
  inp <- H.getInputLine ">>> "
  let cmd = fromMaybe "" inp

  -- check here if it's a special command like ':clear' or ':quit'
  case lookup cmd interactiveCommands of
    Just c -> liftIO (void (performAction c)) >> repl
    Nothing -> do
      if cmd == ""
        then do
          liftIO $ putStrLn cmd -- if no input was given, we just skip an continue working in the repl
          repl
        else do
          res <- lift . lift . runExceptT $ P.runREPLPipeline (pack cmd)
          case res of
            Right _ -> return ()
            Left e -> liftIO $ reportError e
          repl

-- | Initialise the REPL by call initPipeline to load in prelude, typecheck it
-- | and get the necesserary language definitions ready when we start running
-- | statements within the interactive mode.
start :: Int -> IO ()
start size = do
  state <- liftIO $ P.initPipeline "<interactive>" size

  liftIO $ putStrLn ver -- print the version
  _ <- runPipeline (H.runInputT suggestionSettings repl) state
  return ()
