{-# LANGUAGE OverloadedStrings #-}
module KubeConfig(kubeClientConfig) where

import System.Directory
import Control.Concurrent.STM (atomically, newTVarIO)
import Data.Map as Map
import Kubernetes.Client      (getContext, KubeConfigSource (..), setMasterURI, applyAuthSettings, newManager, addCACertData, addCACertFile, tlsValidation)
import Network.HTTP.Client (Manager)
import Kubernetes.OpenAPI (KubernetesClientConfig)
import Kubernetes.OpenAPI.Core (newConfig)
import Kubernetes.Client.Auth.OIDC (OIDCCache)

import Kubernetes.Client.KubeConfig
import Kubernetes.Client.Internal.TLSUtils
import qualified Network.TLS as TLS

import Data.Function ( (&) )
import Data.Yaml
import System.FilePath




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
  tlsParams <- configureTLSParams kubeConfig (takeDirectory f) & fmap disableServerNameValidation
  clientConfig <- newConfig & fmap (setMasterURI masterURI) 
  (tlsParamsWithAuth, clientConfigWithAuth) <- case getAuthInfo kubeConfig of
    Left _ -> return (tlsParams, clientConfig)
    Right (_, auth) ->
      applyAuthSettings oidcCache auth (tlsParams, clientConfig)
  mgr <- newManager tlsParamsWithAuth
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
