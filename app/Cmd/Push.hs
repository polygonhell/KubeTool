module Cmd.Push where

import Options.Applicative.Types (Parser)
import Options.Applicative.Simple (optional)
import Options.Applicative
import Text.Printf (printf)
import Environment (Environment(..))
import Project (Project(..))

data Options = Options
  { projectName :: Maybe String
  , optQuiet :: Bool } deriving (Show)

options :: Parser Options
options = Options <$> (optional $ strOption (long "name" <> short 'n' <> help "Name of the Project. Can be omitted if there is a single Project in the Configuration")) 
                  <*> flag False True (short 'b' <> long "foofoo")



pushProject :: Environment -> Project -> IO (Either String ())
pushProject env proj = return $ Right ()


push :: Options -> IO (Either String ())
push opt = do
  putStrLn $ printf "Pushing project %s" (show (projectName opt))
  return $ Right () 