module Cmd.Logs where

import Options.Applicative.Types (Parser)
import Options.Applicative.Simple (optional)
import Options.Applicative
import Control.Monad ( join, forever, void )
import Data.List (find, intercalate, intersect, (\\), isPrefixOf)
import Text.Printf (printf)
import Data.Text (unpack)

import Network.HTTP.Client.Internal (makeLengthReader, getConn, getResponse, Connection(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8




import Config (Config (..), readConfig)
import Environment (Environment (..), readEnvironment)
import qualified Project as P
import qualified Template as T
import Cmd.Utils (withProject, withEnvironment, withRunningPod)
import Kubernetes (execCmd, WSblock(..), withExecCmdConnection, readBlock)
import Kubernetes.OpenAPI (V1Pod (v1PodSpec), v1ObjectMetaName, v1PodMetadata, v1PodStatus, v1PodStatusPhase, V1ObjectMeta (v1ObjectMetaNamespace))



data Options = Options
  { 
    projectName :: Maybe String,
    tailLines :: Maybe Int,
    attach :: Bool
  }
  deriving (Show)

options :: Parser Options
options =
  Options
    <$> optional (strOption (long "name" <> short 'n' <> help "Name of the Project. Can be omitted if there is a single Project in the Configuration"))
    <*> optional (option auto (long "lines" <> short 'l' <> help "Number of lines to log to output"))
    <*> flag False True (short 'f' <> long "follow" <> help "output log as it's appended")



logOutput :: Connection -> IO ()
logOutput con = do
  foo <- readBlock con
  case foo of
    Close ->
      return ()
    Bin bits -> do
      -- first byte designates the output channel
      putStr $ C8.unpack bits
      logOutput con
    _  -> error "unsupported packet seen"


logs :: Options -> IO (Either String ())
logs opt = 
  withProject (projectName opt) $ \p -> do
    let name = P.name p
    withRunningPod name $ \pod -> do
      let detailsO = do
                  spec <- v1PodMetadata pod
                  ns <- v1ObjectMetaNamespace spec
                  name <- v1ObjectMetaName spec
                  return (unpack name, unpack ns)

      case detailsO of
        Nothing -> return $ Left "Couldn't get pod details"
        Just (podName, ns) -> do
          putStrLn $ printf "Logging project %s" $ P.name p
          putStrLn $ printf "project %s" $ show p
          let cmd = "tail -f -n 20 /tmp/run"
          withExecCmdConnection ns podName ["bash", "-c", cmd] $ \connection -> do
            logOutput connection
            return $ Right ()



