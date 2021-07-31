{-# LANGUAGE OverloadedStrings #-}
module KubeConfig(kubeClientConfig) where

import System.Directory
import Control.Concurrent.STM (atomically, newTVarIO)
import Data.Map as Map
import Kubernetes.Client      (getContext, KubeConfigSource (..), setMasterURI, applyAuthSettings, addCACertData, addCACertFile, tlsValidation)
import Network.HTTP.Client (Manager, newManager, ManagerSettings (managerRawConnection), rawConnectionModifySocket)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.Connection ( TLSSettings(..) )
import Network.Socket ( setSocketOption, SocketOption(..) )
-- import Network.Socket.Options (SockOpt)
import Kubernetes.OpenAPI (KubernetesClientConfig)
import Kubernetes.OpenAPI.Core (newConfig)
import Kubernetes.Client.Auth.OIDC (OIDCCache)

import Kubernetes.Client.KubeConfig
import Kubernetes.Client.Internal.TLSUtils
import qualified Network.TLS as TLS

import Data.Function ( (&) )
import Data.Yaml
import System.FilePath

keepIdle = CustomSockOpt  (6, 4)
keepInterval = CustomSockOpt (6, 5)
keepCount = CustomSockOpt (6, 6)

{-|
  Creates 'NH.Manager' and 'K.KubernetesClientConfig' for a given
  'KubeConfigSource'. It is recommended that multiple 'kubeClient' invocations
  across an application share an 'OIDCCache', this makes sure updation of OAuth
  token is synchronized across all the different clients being used.
-}
mkKubeClientConfig
  :: OIDCCache -> KubeConfigSource -> IO (Manager, KubernetesClientConfig)
mkKubeClientConfig oidcCache (KubeConfigFile f) = do
  kubeConfig <- decodeFileThrow f
  masterURI  <-
    server
    <$> getCluster kubeConfig
    &   either (const $ pure "localhost:8080") return
  
  tlsParams <- configureTLSParams kubeConfig (takeDirectory f) 
    & fmap disableServerNameValidation

  clientConfig <- newConfig & fmap (setMasterURI masterURI) 
  (tlsParamsWithAuth, clientConfigWithAuth) <- case getAuthInfo kubeConfig of
    Left _ -> return (tlsParams, clientConfig)
    Right (_, auth) ->
      applyAuthSettings oidcCache auth (tlsParams, clientConfig)

  let settings = mkManagerSettings (TLSSettings tlsParamsWithAuth) Nothing
  let modifySO = do
        rawConnectionModifySocket $ \s -> do
          setSocketOption s keepIdle 11
          setSocketOption s keepInterval 10
          setSocketOption s keepCount 2
          setSocketOption s KeepAlive 1

  let settingsWithSO = settings { managerRawConnection = modifySO }

  mgr <- newManager settingsWithSO
  return (mgr, clientConfigWithAuth)
  
-- mkKubeClientConfig _ KubeConfigCluster = mkInClusterClientConfig
mkKubeClientConfig _ KubeConfigCluster = error "ClusterClientConfig without configFile not supported"

configureTLSParams :: Config -> FilePath -> IO TLS.ClientParams
configureTLSParams cfg dir = do
  defaultTLS     <- defaultTLSClientParams
  withCACertData <- addCACertData cfg defaultTLS
  withCACertFile <- addCACertFile cfg dir withCACertData
  return $ tlsValidation cfg withCACertFile


configFileName :: IO String
configFileName = do 
  homeDir <- getHomeDirectory
  return $ homeDir ++ "/.kube/config"






kubeClientConfig :: IO (Manager, KubernetesClientConfig)
kubeClientConfig = do
  fileName <- configFileName
  oidcCache <- newTVarIO Map.empty
  (mgr, kcfg) <- mkKubeClientConfig oidcCache $ KubeConfigFile fileName
  return (mgr, kcfg)
