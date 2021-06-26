module Environment (readEnvironment, writeEnvironment, updateEnvironment, Environment(..)) where


import Data.Yaml (ParseException, ToJSON, FromJSON, decodeFileThrow, decodeFileEither, encodeFile)
import GHC.Generics

data Environment = Environment  { namespace :: !String
                                } deriving (Show, Generic)

instance FromJSON Environment
instance ToJSON Environment

emptyEnvironment :: Environment
emptyEnvironment = Environment ""

environmentName :: String
environmentName = ".kube_env"

readEnvironment :: IO Environment
readEnvironment = do
  environment0 <- decodeFileEither environmentName :: IO (Either ParseException Environment)
  return $ case environment0 of
              Right v -> v
              Left _ -> emptyEnvironment

writeEnvironment :: Environment -> IO Environment
writeEnvironment environment = do
  encodeFile environmentName environment
  return environment

updateEnvironment :: (Environment -> Environment) -> IO Environment
updateEnvironment fn = do
  environmentIn <- readEnvironment
  writeEnvironment  (fn environmentIn)
