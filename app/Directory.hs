module Directory where

import Crypto.Hash.MD5 (hash)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Numeric (showHex)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.FilePath ((</>))
import Text.Printf (printf)
import Text.Regex.TDFA ((=~))
import Data.List ((\\), intercalate, isPrefixOf)
import System.Process

filterFileList :: (FilePath -> IO Bool) -> [FilePath] -> IO [FilePath]
filterFileList fn ps = do
  val <- mapM (exists fn) ps
  return $ concat val
  where
    exists :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
    exists fn p = do
      e <- fn p
      return $ if e then [p] else []

exclusions = ["node_modules.*", "build.*"]

-- Given a set of paths return the list of paths of files contained
getFiles :: [String] -> [FilePath] -> IO [FilePath]
getFiles excl [] = return []
getFiles excl ps = do
  let exclusions = fixExclusions excl
  print ps
  files <- filterFileList doesFileExist ps
  dirs <- filterFileList doesDirectoryExist ps

  nested <- do
    n' <- mapM directoryContents dirs
    let n'' = filter (checkExclusions exclusions) (concat n')
    putStrLn $ printf "n'' = %s" $ show n'
    getFiles exclusions n''

  return $ files ++ nested
  where
    fixExclusions :: [String] -> [String]
    fixExclusions es = map ("\\`(\\./)?" ++) exclusions
    directoryContents :: FilePath -> IO [FilePath]
    directoryContents p = do
      names <- listDirectory p
      return $ map (p </>) names
    checkExclusions :: [String] -> FilePath -> Bool
    checkExclusions es p = all (checkOne p) es
    checkOne :: FilePath -> String -> Bool
    checkOne p e = not (p =~ e)

-- Returns the md5's as printed strings to make it easier to compare to results of md5sum
computeMd5s :: [FilePath] -> IO [(FilePath, String)]
computeMd5s ps = mapM computeMd5 ps
  where
    computeMd5 :: FilePath -> IO (FilePath, String)
    computeMd5 p = do
      bs <- BS.readFile p
      return (p, hex $ hash bs)
    hex :: BS.ByteString -> String
    hex = Char8.unpack . toLazyByteString . byteStringHex


-- Soultion uses find on client and server for consistency - will have to be changed for Windows

findCommand :: String -> [String] -> [FilePath] -> IO String
findCommand com excl paths = do
  -- really should escape these
  files <- filterFileList doesFileExist paths
  dirs <- filterFileList doesDirectoryExist paths
  -- Directory pattens need to end with a /
  let includeTerms = map buildRegexF files ++ map buildRegexD dirs
  let include = intercalate "|" includeTerms

  let excludeTerms =  map ("(\\./)?" ++) excl
  let exclude = intercalate "|" excludeTerms
  let res = printf "find . -regextype posix-extended -type f -regex \"%s\" -not -regex \"%s\" %s" include exclude com
  return res
  where
    terminateDir d = if (last d) == '/' then d else d ++ "/"
    buildRegexF s = "(\\./)?" ++ s
    buildRegexD "." = "\\./" ++ ".*"
    buildRegexD s = "(\\./)?" ++ terminateDir s ++ ".*"


data CheckSum = CheckSum  { name:: String
                          , md5:: String
                          } deriving (Show, Eq)

packageFindResult :: String -> [CheckSum]
packageFindResult str = res where
  res =  map partition $ lines str
  partition :: String -> CheckSum
  partition l = CheckSum fname md5 where
    (md5, fname') = break (\x -> x == ' ') l
    fname'' = dropWhile (\x -> x == ' ') fname'
    fname:: String
    fname = if "./" `isPrefixOf` fname'' then drop 2 fname'' else fname''



runFindMd5 :: [String] -> [FilePath] -> IO [CheckSum]
runFindMd5 excl paths = do
  find <- findCommand "-exec md5sum {} \\;" excl paths
  out <- readProcess "bash" ["-c", find] ""
  return $ packageFindResult out



