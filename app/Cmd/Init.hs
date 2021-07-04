module Cmd.Init where

import Options.Applicative.Types (Parser)
import Options.Applicative.Simple (optional)
import Options.Applicative
import Data.Yaml (ToJSON, FromJSON, decodeThrow, encode)
import System.Directory (doesFileExist)



import qualified Template as T
import qualified Project as P
import qualified Config as C


data Options = Options
  { projectName :: String
  , templateName :: String
  }
  deriving (Show)

options :: Parser Options
options =
  Options <$> strOption (long "name" <> short 'n' <> help "Name of the initial Project.")
    <*> strOption (long "template" <> short 't' <> help "Name of the initial Projects template.")




init :: Options -> IO (Either String ())
init opt = do
  exists <- doesFileExist C.configName
  if exists then return $ Left "Error: Config file exists and would be overwritten" else (do
    let templates = T.defaultTemplates
    let templateExists = templateName opt `elem` map T.name templates
    let project = P.Project (projectName opt) (templateName opt) []
    let config = C.Config "name" Nothing [project] templates
    if templateExists then (do
      C.writeConfig config
      return $ Right ()
      ) else return $ Left "Error: Template is not available"
    )

