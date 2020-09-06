module Cmd.Namespace where


import qualified Data.Map as Map
import Data.Text (unpack, Text)
import Kubernetes.OpenAPI     (dispatchLbs, ContentType(..), Accept (..), MimeError(..), MimeJSON (..), MimeResult (..), V1NamespaceList(..), V1Namespace(..), V1ObjectMeta(..), mkV1Namespace, mkV1ObjectMeta, dispatchMime)
import qualified Kubernetes.OpenAPI.API.CoreV1 as CoreV1
import Kubernetes.Client      (getContext, KubeConfigSource (..), mkKubeClientConfig)
import ErrorResponse ( ErrorResponse(message) )
import Data.Yaml (decodeFileThrow)
import Control.Concurrent.STM (atomically, newTVar)
import Data.Aeson (eitherDecode)
import Text.Printf (printf)
import Kubernetes.OpenAPI.Model (Name(Name))
import Network.HTTP.Types (Status(..), status200)
import Network.HTTP.Client (Response(..))
import KubeConfig ( kubeClientConfig )
import Data.Maybe (fromJust)
import Kubernetes.OpenAPI (V1NamespaceStatus(v1NamespaceStatusPhase))
import Cmd.Env(set)
import qualified Environment as E
import qualified Kubernetes as K

create :: Text -> IO (Either String ())
create name = do
  -- config <- decodeFileThrow  "/home/rob/.kube/config" :: IO Config
  -- let Right context = getContext config
  -- print $ namespace context
  (mgr, kcfg) <- kubeClientConfig
  let ns = mkV1Namespace { v1NamespaceMetadata = Just (mkV1ObjectMeta { v1ObjectMetaName = Just name }) }
  res2 <- dispatchMime mgr kcfg (CoreV1.createNamespace (ContentType MimeJSON) (Accept MimeJSON) ns)
  -- Parse the error string out of the HTTP response
  case res2 of 
    (MimeResult (Left (MimeError str _)) resp) -> return $ Left $ printf "%s -- %s" str (message er)
      where
        er :: ErrorResponse
        Right er = eitherDecode  $ responseBody resp
    (MimeResult (Right _) _) -> set $ printf "namespace=%s" (unpack name)


delete :: Text -> IO (Either String ())
delete name = do
  (mgr, kcfg) <- kubeClientConfig
  resp <- dispatchLbs mgr kcfg (CoreV1.deleteNamespace (ContentType MimeJSON) (Accept MimeJSON) (Name name))
  -- Note Mime response not parsed properly by the library
  -- Parse the error string out of the HTTP response
  let status = statusCode (responseStatus resp)
  let strOut = if (status >= 200 && status <=202) then
                  Right ()
               else
                  Left $ message er 
                  where
                    er :: ErrorResponse
                    Right er = eitherDecode  $ responseBody resp                                  
  return strOut



list :: IO (Either String ())
list = do
  (mgr, kcfg) <- kubeClientConfig
  env <- E.readEnvironment
  namespaces <- K.listNamespaces
  let currentNS = E.namespace env
  res2 <- dispatchMime mgr kcfg (CoreV1.listNamespace (Accept MimeJSON))
  case namespaces of 
    Left str -> return $ Left str
    Right nss -> do
      let metaDatas = map (fromJust . v1NamespaceMetadata) nss
      let names = map (unpack . fromJust . v1ObjectMetaName) metaDatas
      let cleanStatus x = if x == "Active" then "" else " (" ++ x ++ ")"
      let status = map (cleanStatus . unpack . fromJust . v1NamespaceStatusPhase . fromJust . v1NamespaceStatus) nss
      let toString (n, s) = printf "%s%s%s" (if n == currentNS then "* " else "") n s :: String
      let strs = map toString $ zip names status
      putStr $ unlines strs
      return $ Right ()
