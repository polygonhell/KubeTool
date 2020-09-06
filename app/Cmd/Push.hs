module Cmd.Push where

import Options.Applicative.Types (Parser)
import Options.Applicative.Simple (optional)
import Options.Applicative
import Text.Printf (printf)
import Environment (readEnvironment, Environment(..))
import Project as P ( Project(name) )
import Project (Project(..))
import Config (Config(..), readConfig)
import Data.List(intercalate, find)
import Text.Printf(printf)

data Options = Options
  { projectName :: Maybe String
  , optQuiet :: Bool } deriving (Show)

options :: Parser Options
options = Options <$> (optional $ strOption (long "name" <> short 'n' <> help "Name of the Project. Can be omitted if there is a single Project in the Configuration")) 
                  <*> flag False True (short 'b' <> long "foofoo")



pushProject :: Environment -> Config -> Project -> IO (Either String ())
pushProject env config proj = 
  return $ Left "ffffff"


push :: Options -> IO (Either String ())
push opt = do
  env <- readEnvironment
  config <- readConfig
  let ps' = case projectName opt of
              Nothing -> Right $ projects config
              Just n -> case find (\x -> P.name x == n) (projects config) of
                          Nothing -> Left $ (printf "Bad Project Name %s" n :: String)
                          Just p -> Right [p]
  case ps' of
    Left e -> return $ Left e
    Right ps -> do
                  putStrLn $ printf "Pushing projects %s" $ intercalate ", " (map P.name ps)
                  foo <- mapM (pushProject env config) ps
                  return $ fmap (\x -> ()) $ sequence foo

  