module Cmd.Project where

import Options.Applicative.Types (Parser)
import Options.Applicative.Simple (optional)
import Options.Applicative

import Config as C
import Project as P
import Data.Map((!?))
import qualified Data.Map as Map
import Data.List (find)
import Text.Printf (printf)
import Data.Maybe (isJust)

data Options = Options
  { projectName :: Maybe String
  , optQuiet :: Bool } deriving (Show)

runCmd :: (Options, Options -> IO (Either String ())) -> IO (Either String ())
runCmd (opts, subCmd) = subCmd opts

options :: Parser Options
options = Options <$> (optional $ strOption (long "name" <> short 'n' <> help "Name of the Project. Can be omitted if there is a single Project in the Configuration")) 
                  <*> flag False True (short 'b' <> long "foofoo")

set :: String -> Options -> IO (Either String ())
set v o = do 
  config <- readConfig
  let ps = map (\x -> (P.name x, x)) $ projects config
  let projectO = case (length ps, projectName o) of
                  (0, _) -> Nothing
                  (_, Just(name)) -> find (\x -> name == fst x) ps
                  (1, Nothing) -> Just $ head ps
                  (_, _) -> Nothing

  newProject <- case projectO of
                          Nothing -> return $ Left "Invalid project specified, must specify a Project name if more than one in current Configuration"
                          Just (n, p) -> do
                            putStrLn $ printf "Updating project %s" n
                            case name of 
                              "name" -> return  if (n /= value && isJust (find (\x -> value == fst x) ps)) then
                                                  Left "Can't set name to existing project name"
                                                else
                                                  Right (n, p {P.name = value})
                              err -> return $ Left $ "Unknown variable to set " ++ err
                            where
                              (name, value') = span (\x -> x /= '=') v 
                              value = tail value'

  case newProject of
    Left err -> return $ Left err
    Right (on, np) -> do
      let otherProjects = filter (\x -> on /= fst x) ps
      let projects = np : map snd otherProjects
      C.writeConfig $ config {projects = projects}
      return $ Right ()


elipseStr :: Int -> String -> String
elipseStr n str = if length str <= n then
                    str
                  else
                    take (n-3) str ++ "..."


list ::  Options -> IO (Either String ())
list o = do
  config <- readConfig
  let ps = projects config
  putStrLn $ printf format "Name" "Template"
  mapM_ pPrint ps
  return $ Right ()
  where
    format = "%-20s %-30s"
    pPrint :: Project -> IO ()
    pPrint p = do
      let name = elipseStr 20 (P.name p)
      let template = elipseStr 20 (P.template p)
      putStrLn $ printf format name template
    

