module Template (Template(..), templatesCmd, templateBS, Wrapper(..), options) where

import Options.Applicative.Types (Parser)
import Options.Applicative.Simple (optional)
import Options.Applicative
import Data.ByteString (ByteString)

import GHC.Generics

import Cmd.Templates.CreateReactApp
import Data.Yaml (ToJSON, FromJSON, decodeThrow)
import Data.ByteString.UTF8 (fromString)
data Template = Template { name :: !String
                         , buildContainer :: !String
                         , deployContainer :: !String
                         , ports :: ![Int]
                         , sourceFiles :: ![String]
                         , runDevCommand :: !String
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

newtype Wrapper = Wrapper { templates :: [Template] }  deriving (Show, Generic)
instance FromJSON Wrapper

templateBS :: ByteString
templateBS = fromString (unlines [
  "templates:",
  createReactApp
  ])

templatesCmd :: Options -> IO (Either String ())
templatesCmd opt = do
  wrapper <- decodeThrow templateBS :: IO Wrapper
  let names = map name (templates wrapper)
  putStrLn "Available templates"
  mapM_ putStrLn names
  return $ Right $ ()