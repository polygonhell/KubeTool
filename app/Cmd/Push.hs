{-# LANGUAGE OverloadedStrings #-}

module Cmd.Push where

import Config (Config (..), readConfig)
import Control.Concurrent (threadDelay)
import Control.Monad (join)
import Data.List (find, intercalate)
import Data.Maybe (fromJust)
import Data.Text (unpack)
import Environment (Environment (..), readEnvironment)
import Kubernetes
import Kubernetes.OpenAPI (V1Pod (v1PodSpec), v1ObjectMetaName, v1PodMetadata, v1PodStatus, v1PodStatusPhase)
import Options.Applicative
import Options.Applicative.Simple (optional)
import Options.Applicative.Types (Parser)
import Project (Project (..), serviceFromProject, statefulSetFromProject)
import qualified Project as P
import System.Directory (getCurrentDirectory)
import Template (Template)
import qualified Template as T
import Text.Printf (printf)

data Options = Options
  { projectName :: Maybe String,
    optQuiet :: Bool
  }
  deriving (Show)

options :: Parser Options
options =
  Options <$> (optional $ strOption (long "name" <> short 'n' <> help "Name of the Project. Can be omitted if there is a single Project in the Configuration"))
    <*> flag False True (short 'b' <> long "foofoo")

pushDevContainer :: Environment -> Config -> Project -> Template -> IO (Either String String)
pushDevContainer env config proj template = do
  let ns = namespace env
  let name = P.name proj
  podsO <- getPodsWithName ns name
  -- TODO deal with multiple instances case
  putStrLn $ printf "podsO = %s" (show podsO)
  case podsO of
    Right (pod : []) -> do
      -- Check Pod is running
      let status = fromJust $ v1PodStatusPhase (fromJust $ v1PodStatus pod)
      case status of
        "Running" -> do
          let podName = do
                spec <- v1PodMetadata pod
                v1ObjectMetaName spec
          putStrLn $ printf "Pod Running"
          return case podName of
            Nothing -> Left "Couldn't get podName"
            Just name -> Right $ unpack name
        status -> do
          putStrLn $ printf "Waiting for Pod (%s)" status
          threadDelay 1000000
          pushDevContainer env config proj template
    Right [] -> do
      -- Need to create the Stateful Set and Service
      let serviceDef = serviceFromProject env proj
      putStrLn "Creating the Service"
      service <- createService ns serviceDef
      let ssDef = statefulSetFromProject env proj template
      putStrLn "Creating the StatefulSet"
      ss <- createStatefulSet (namespace env) ssDef
      pod <- sequence do
        service
        ss
        return $ pushDevContainer env config proj template

      return $ join pod
    Right _ -> return $ Left "More than one build pod found"
    Left err -> return $ Left err

filesToCopy :: Project -> [String]
filesToCopy _ = ["app", "package.json", "tsconfig.json"]

pushProject :: Environment -> Config -> Project -> Template -> IO (Either String String)
pushProject env config proj template = do
  putStrLn $ printf "Pushing project"
  let ns = namespace env
  baseDir <- getCurrentDirectory
  let files = filesToCopy proj
  let dest = "/project"
  podName <- pushDevContainer env config proj template
  copied <- fmap join $ sequence $ fmap (\x -> copy ns x baseDir files dest) podName

  print copied

  -- todo build and restart
  ran <- fmap join $ sequence $ fmap (\x -> execCmd ns x ["bash", "-c", "echo 'FOO=1234 env' > cmd && supervisorctl -s unix:///tmp/supervisor.sock start run"]) podName

  return $ do
    copied
    ran

push :: Options -> IO (Either String ())
push opt = do
  env <- readEnvironment
  config <- readConfig
  let ps' = case projectName opt of
        Nothing -> Right $ projects config
        Just n -> case find (\x -> P.name x == n) (projects config) of
          Nothing -> Left $ (printf "Bad Project Name %s" n :: String)
          Just p -> Right [p]

  a <- case ps' of
    Left e -> return $ Left e
    Right ps -> do
      let ts = templates config
      let foo = sequence $ map (\p -> fmap (\t -> (p, t)) (find (\t -> T.name t == P.template p) ts)) ps
      case sequence $ map (\p -> fmap (\t -> (p, t)) (find (\t -> T.name t == P.template p) ts)) ps of
        Just xs -> do
          putStrLn $ printf "Pushing projects %s" $ intercalate ", " (map P.name ps)
          foo <- mapM (\(p, t) -> pushProject env config p t) xs
          return $ fmap (\x -> ()) $ sequence foo
        Nothing -> return $ Left "Referenced template missing"

  return a
