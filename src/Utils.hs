-- | Hash compiler utilities.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module Utils where

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Embed (standardLibrary)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist)
import System.FilePath (hasExtension, joinPath, normalise, takeExtension)

-- | Fast vector uncons operation, when we know that performing this operation
-- | is ok to do. This is mainly used in the 'evalInstruction' within the
-- | Runtime.
uncons :: V.Vector a -> Maybe (a, V.Vector a)
uncons v =
  if V.null v
    then Nothing
    else Just (V.unsafeHead v, V.unsafeTail v)

-- | Utility function to wrap some list with two of the same element. This is particularly
-- | handy when you want to work with strings and quouting them.
wrap :: a -> [a] -> [a]
wrap x xs = x : xs ++ [x]

-- | Utility function to take two lists of potentially un-equal length and zip it
-- | so that the smaller list is filled with values of 'Nothing' rather than just
-- | truncating it.
zipMaybe :: [a] -> [b] -> [(Maybe a, Maybe b)]
zipMaybe (x : xs) (y : ys) = (Just x, Just y) : zipMaybe xs ys
zipMaybe xs [] = [(Just x, Nothing) | x <- xs]
zipMaybe [] ys = [(Nothing, Just y) | y <- ys]

-- | Simply checks if the given file path has an extension "hash"
hasHashExtension :: FilePath -> Bool
hasHashExtension path = takeExtension path == ".hash"

-- | This function is used to resolve a given module reference name within the
-- | given file path context.
resolveModPath :: FilePath -> FilePath -> IO FilePath
resolveModPath wd filepath = do
  -- check if we're accesing a standard library module here
  ( if filepath `HM.member` standardLibrary
      then return filepath
      else
        ( do
            -- we know that 'wd' exist since it is checked earlier in Main
            let joined = joinPath [wd, filepath]

            -- if the joined path is a directory, we automatically assume that the
            -- import is meant to add a "<dir>/index.hash", otherwise we add the ".hash"
            -- extension if it already doesn't exist.
            exists <- doesDirectoryExist joined
            fExists <- doesFileExist joined

            if exists
              then return . normalise $ joinPath [joined, "index.hash"]
              else
                if hasHashExtension joined
                  then return . normalise $ joined
                  else do
                    if hasExtension joined && fExists --  don't try to append .hash if the file path already has
                      then return . normalise $ joined
                      else return . normalise $ joined ++ ".hash"
        )
    )

-- | Get the absolute module path by using the name of the module and the prefix
-- | directory.
absModulePath :: FilePath -> FilePath -> FilePath
absModulePath prefix modName = normalise . joinPath $ [prefix, modName]

-- | Call 'resolveModPath' on the current working directory.
resolvePath :: FilePath -> IO FilePath
resolvePath = resolveModPath "."

-- | Utility function to normalise a specified path, normalising invloves canonicalising
-- | the path if it uses a special syntax within Unix/Windows and getting the absolute
-- | path. Secondly, this check if the given path exists and fails if it cannot find the
-- | specified path.
normalisePath :: FilePath -> IO FilePath
normalisePath path = do
  normalised <- canonicalizePath path
  exists <- doesDirectoryExist normalised

  if exists then return normalised else fail $ "Include directory '" ++ path ++ "' does not exist."
