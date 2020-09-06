module KubeConfig(kubeClientConfig) where

import System.Directory
import Control.Concurrent.STM (atomically, newTVar)
import Data.Map as Map
import Kubernetes.Client      (getContext, KubeConfigSource (..), mkKubeClientConfig)
import Network.HTTP.Client (Manager)
import Kubernetes.OpenAPI (KubernetesClientConfig)


configFileName :: IO String
configFileName = do 
  homeDir <- getHomeDirectory
  return $ homeDir ++ "/.kube/config"

kubeClientConfig :: IO (Manager, KubernetesClientConfig)
kubeClientConfig = do
  fileName <- configFileName
  oidcCache <- atomically $ newTVar $ Map.fromList []
  (mgr, kcfg) <- mkKubeClientConfig oidcCache $ KubeConfigFile fileName
  return (mgr, kcfg)
