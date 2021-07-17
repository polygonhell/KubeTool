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
import Cmd.Utils (withProject, withEnvironment, withRunningPod, withRunningPodNames)
import Kubernetes (execCmd, WSblock(..), withExecCmdConnection, readBlock)
import Kubernetes.OpenAPI (V1Pod (v1PodSpec), v1ObjectMetaName, v1PodMetadata, v1PodStatus, v1PodStatusPhase, V1ObjectMeta (v1ObjectMetaNamespace))
import Data.Maybe (fromJust, fromMaybe)



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
    withRunningPodNames name $ \ns podName -> do
      let att = if attach opt then "-f" else ""
      let lines = fromMaybe 1000 (tailLines opt)
      let cmd = printf "tail %s -n %d /tmp/run" att lines
      withExecCmdConnection ns podName ["bash", "-c", cmd] $ \connection -> do
        logOutput connection
        return $ Right ()



