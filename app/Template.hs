module Template (Template(..), templatesCmd, defaultTemplates, options) where

import Options.Applicative.Types (Parser)
import Options.Applicative.Simple (optional)
import Options.Applicative
import Data.ByteString (ByteString)

import GHC.Generics

import Data.Yaml (ToJSON, FromJSON, decodeThrow)
import Data.ByteString.UTF8 (fromString)
data Template = Template { name :: !String
                         , buildContainer :: !String
                         , deployContainer :: !String
                         , ports :: ![Int]
                         , sourceFiles :: ![String]
                         , pushCommand :: !String
                         , watchCommand :: Maybe String
                         } deriving (Show, Generic)

instance FromJSON Template
instance ToJSON Template


newtype Options = Options
  { verbose :: Bool
  }
  deriving (Show)

options :: Parser Options
options =
  Options <$> flag False True (short 'v' <> long "verbose")

-- newtype Wrapper = Wrapper { templates :: [Template] }  deriving (Show, Generic)
-- instance FromJSON Wrapper

defaultTemplates = [
  Template
    "TSCreateReactApp"
    "polygonhell/nodejs-kt:latest"
    "someOtherURL"
    [3000]
    ["src", "public", "package.json", "package-lock.json", "tsconfig.json"]
    "npm i && npm run start"
    Nothing
  ]



templatesCmd :: Options -> IO (Either String ())
templatesCmd opt = do
  let names = map name defaultTemplates
  putStrLn "Available templates"
  mapM_ putStrLn names
  return $ Right ()