module Config (readConfig, writeConfig, updateConfig, Config(..)) where


import Data.Yaml (ToJSON, FromJSON, decodeFileThrow, encodeFile)
import GHC.Generics
import Project
import Template


data Config = Config { name :: !String
                     , author :: Maybe String
                     , projects :: ![Project]
                     , templates :: ![Template]

                     } deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config


configName :: String
configName = "config.yaml"

readConfig :: IO Config
readConfig = do
  config <- decodeFileThrow configName :: IO Config
  return config

writeConfig :: Config -> IO Config
writeConfig config = do
  encodeFile configName config
  return config

updateConfig :: (Config -> Config) -> IO Config
updateConfig fn = do
  configIn <- readConfig
  writeConfig  (fn configIn)