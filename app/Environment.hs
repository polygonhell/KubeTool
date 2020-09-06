module Environment (readEnvironment, writeEnvironment, updateEnvironment, Environment(..)) where


import Data.Yaml (ParseException, ToJSON, FromJSON, decodeFileThrow, decodeFileEither, encodeFile)
import GHC.Generics
import Template
import Data.Maybe (fromMaybe)

data FileAndVer = FileAndVer  { name :: !String
                              , ver :: !String 
                              } deriving (Show, Generic)

data Environment = Environment  { namespace :: !String
                                , fileList :: ![FileAndVer]
                                } deriving (Show, Generic)

instance FromJSON FileAndVer
instance ToJSON FileAndVer
instance FromJSON Environment
instance ToJSON Environment

emptyEnvironment :: Environment
emptyEnvironment = Environment "" []

environmentName :: String
environmentName = ".kube_env"

readEnvironment :: IO Environment
readEnvironment = do
  environment0 <- decodeFileEither environmentName :: IO (Either ParseException Environment)
  return $ case environment0 of
              Right v -> v
              Left v -> emptyEnvironment

writeEnvironment :: Environment -> IO Environment
writeEnvironment environment = do
  encodeFile environmentName environment
  return environment

updateEnvironment :: (Environment -> Environment) -> IO Environment
updateEnvironment fn = do
  environmentIn <- readEnvironment
  writeEnvironment  (fn environmentIn)
