-- | Hash error reporting module, providing some useful functions to report
-- | issues with command line arguments, program syntax, semantics, and
-- | any other errors.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module Report (reportErrorWithTokens, reportError, reportIOError) where

import Data.List (intercalate)
import System.IO.Error (ioeGetErrorString)
import Utils (wrap)

-- What colour to print a given prefix or a token.
data Colour
  = Red
  | Blue
  | Yellow
  | None

-- Simple colour data representation to use when working with formatting
-- Error messages.
data PrettyText = PrettyText
  { colour :: Colour,
    bold :: Bool
  }

-- | Convert a 'Colour' data type to a ANSI colour code.
fromColour :: Colour -> String
fromColour Red = "\x1b[31m"
fromColour Blue = "\x1b[34m"
fromColour Yellow = "\x1b[35"
fromColour None = ""

-- | emphasise a colour using the bold escape ANSI sequence
boldEmpahisis :: [Char]
boldEmpahisis = "\x1b[1m"

-- | The console code to represent an 'info' colour prefix which is currently blue
-- infoColourPrefix :: [Char]
-- infoColourPrefix = fromColour Blue ++ boldEmpahisis

-- | The console code to represent an 'warn' colour prefix which is currently magenta
-- warningColourPrefix :: [Char]
-- warningColourPrefix = fromColour Yellow ++ boldEmpahisis

-- | The console code to represent an 'error' colour prefix which is currently red
errorColourPrefix :: [Char]
errorColourPrefix = fromColour Red ++ boldEmpahisis

-- | Convert a 'PrettyText' into an ANSI string
colToEsc :: PrettyText -> String
colToEsc (PrettyText c b) = fromColour c ++ if b then boldEmpahisis else ""

-- | Surround a string with a 'bold' colour using ANSI
emphasiseToken :: PrettyText -> String -> String
emphasiseToken col token = wrap '\'' $ colToEsc col ++ token ++ resetColour

-- | Utility function to add the 'error' prefix to error messages
errPrefix :: [Char]
errPrefix = errorColourPrefix ++ "error" ++ resetColour ++ ": "

-- | Resets any colour prefix
resetColour :: [Char]
resetColour = "\x1b[0m"

-- | Error reporting function used to report an IO error, mainly used when processing
-- | commandline arguments/setup in Main.
reportIOError :: IOError -> IO ()
reportIOError err = reportError $ ioeGetErrorString err

-- | Simple function to print an error with a command line argument
-- | specified for to the interpreter.
reportError :: String -> IO ()
reportError err = putStrLn $ errorColourPrefix ++ "error" ++ resetColour ++ ": " ++ err

-- | Report an error with some tokens which are emphasised in the bold colour.
reportErrorWithTokens :: String -> [String] -> String
reportErrorWithTokens err tokens = errPrefix ++ err ++ intercalate " or " (map (emphasiseToken PrettyText {colour = None, bold = True}) tokens)
