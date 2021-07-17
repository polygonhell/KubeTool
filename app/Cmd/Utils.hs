{-# LANGUAGE OverloadedStrings #-}
module Cmd.Utils where

import Options.Applicative.Types (Parser)
import Options.Applicative.Simple (optional)
import Options.Applicative
import Data.List (find, intercalate, intersect, (\\), isPrefixOf)
import Data.Maybe (fromMaybe, fromJust)
import Data.Text (Text, pack, unpack)
import Text.Printf (printf)


import Config (Config (..), readConfig)
import Environment (Environment (..), readEnvironment)
import qualified Project as P
import qualified Template as T
import Kubernetes.OpenAPI.Model (V1Pod(..), v1PodStatusPhase, v1ObjectMetaName)
import Kubernetes (getPodsWithName)
import Control.Concurrent (threadDelay)


withEnvironment :: (Environment -> IO a) -> IO a
withEnvironment fn = do
  env <- readEnvironment
  fn env

withConfig :: (Config -> IO a) -> IO a
withConfig fn = do
  config <- readConfig
  fn config

withProject :: Maybe String -> (P.Project -> IO (Either String a)) -> IO (Either String a)
withProject projectName fn = do
  withConfig $ \config -> do
    let prjs = projects config
    let p' = case projectName of
          Nothing -> if length prjs == 1 then Right (head prjs) else Left "Must Specify project name if config contains more than 1"
          Just n -> case find (\x -> P.name x == n) prjs of
            Nothing -> Left (printf "Bad Project Name %s" n :: String)
            Just p -> Right p

    case p' of
      Left e -> return $ Left e
      Right p -> fn p


withRunningPodI :: Environment -> Config ->  String -> (V1Pod -> IO (Either String a)) -> IO (Either String a)
withRunningPodI env config projectName fn = do
  let ns = namespace env
  podsO <- getPodsWithName ns projectName
  -- TODO deal with multiple instances case
  -- putStrLn $ printf "podsO = %s" (show podsO)
  case podsO of
    Right [pod] -> do
      -- Check Pod is running
      let status = fromJust $ v1PodStatusPhase (fromJust $ v1PodStatus pod)
      case status of
        "Running" -> fn pod
        status -> do
          putStrLn $ printf "Waiting for Pod (%s)" status
          threadDelay 1000000
          withRunningPodI env config projectName fn

    Right [] -> return $ Left "No Pods Found"
    Right pss -> return $ Left "Multiple Pods Found"
    Left err -> return $ Left err

withRunningPod :: String -> (V1Pod -> IO (Either String a)) -> IO (Either String a)
withRunningPod projectName fn = 
  withEnvironment $ \env -> 
  withConfig $ \config -> withRunningPodI env config projectName fn