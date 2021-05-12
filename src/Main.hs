{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

-- | Hash runtime entry point. Handle arguments and run the program.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module Main where

import Data.Char (isSpace)
import Data.Functor ((<&>))
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Interpreter
import Pipeline (initPipeline, runModPipeline, runPipeline)
import Report (reportError, reportIOError)
import System.Console.CmdArgs
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (takeDirectory)
import System.IO.Error (catchIOError)
import Utils (hasHashExtension, normalisePath, resolvePath)

-- | Definition for the hash executable arguments
data Arguments = Arguments
  { execute :: FilePath,
    includes :: [FilePath],
    stackSize :: Int
  }
  deriving (Show, Data, Typeable)

-- | Setup the arguments parser for hash
arguments :: Arguments
arguments =
  Arguments
    { includes = ["."] &= name "i" &= typDir &= help "Include a directory into runtime. The current directory is included by default",
      execute = def &= name "e" &= typFile &= help "Execute the passed script directly without launching interactive mode",
      stackSize = 10000 &= name "s" &= typ "INT" &= help "Set the maximum stack size for the current running instance."
    }
    &= program "hash"
    &= helpArg [explicit, name "h", name "help", name "?"]
    &= versionArg [explicit, name "v", name "version"]
    &= help "Hash programming language interpreter"
    &= summary (Interpreter.ver ++ ", (C) Hash Language Authors")
    &= details ["Run Hash by simply invoking: ", " hash"]

-- | Entry point of the compiler. Parse arguments, validate them and fill in any defaults. The
-- | either run the REPL pipeline or the module pipeline depending on the provided arguments.
main :: IO ()
main = flip catchIOError reportIOError $ do
  args <- cmdArgs arguments

  -- Run through the provided 'include' directories and check if they exist...
  -- Check that canonical paths all exist, otherwise report error an fail.
  mapM normalisePath (includes args) >>= \includePaths ->
    -- Check if user provides an 'executable' file, and if so does it exist...
    ( if not . all isSpace $ execute args
        then do
          wd <- canonicalizePath "."
          path <- resolvePath =<< canonicalizePath (execute args)

          doesFileExist path >>= \case
            True ->
              if hasHashExtension path
                then do
                  initialState <- initPipeline path (stackSize args)
                  (res, _) <- runPipeline (runModPipeline (takeDirectory path) path includePaths) initialState
                  case res of
                    Right _ -> return ()
                    Left e -> reportError e
                else fail $ "specified file '" ++ execute args ++ "' doesn't have the 'hash' extension"
            _ -> fail $ "could not find '" ++ fromMaybe path (stripPrefix wd path <&> ('.' :)) ++ "' file."
        else Interpreter.start (stackSize args)
    )
