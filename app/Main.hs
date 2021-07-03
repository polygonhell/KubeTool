{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative.Simple
import System.Exit (die)

import qualified Cmd.Env as Env
import qualified Cmd.Config as Config
import qualified Cmd.Namespace as Namespace
import qualified Cmd.Project as Project
import qualified Cmd.Push as Push
import qualified Cmd.Init as Init
import qualified Template

import Directory
import qualified Directory as D

import Config
import Environment
import Project

import qualified Kubernetes as K
import Text.Printf (printf)


-- kube :: IO ()
-- kube = do
--   oidcCache <- atomically $ newTVar $ Map.fromList []
--   (mgr, kcfg) <- mkKubeClientConfig oidcCache $ KubeConfigFile "/home/rob/.kube/config"
--   dispatchMime
--           mgr
--           kcfg
--           (CoreV1.listPodForAllNamespaces (Accept MimeJSON))
--       >>= print



test :: String -> Project.Options -> IO (Either String ())
test a b = return $ Left $ "foo -- " ++ show a ++ " " ++ show b

parse :: IO (Either String ())
parse = do  (opts,runCmd) <-
              simpleOptions "1.0.0" "header" "desc" (pure ())
              do addSubCommands "config" "configuration commands"
                   do addCommand "set" "set global configuration values" Config.set (strArgument (metavar "value" <> help "name=value etc."))
                 addSubCommands "env" "environment commands"
                   do addCommand "set" "set environment configuration values" Env.set (strArgument (metavar "value" <> help "namespace=value"))
                 addSubCommands "namespace" "Namespace management (list, create, delete)"
                   do addCommand "create" "Create a K8's Namespace" Namespace.create (strArgument (metavar "name" <> help "Name of the namespace to be created"))
                      addCommand "delete" "Delete a K8's Namespace" Namespace.delete (strArgument (metavar "name" <> help "Name of the namespace to be deleted"))
                      addCommand "list" "list available K8's Namespaces" (const Namespace.list) (pure ())
                 addCommand "project" "Project commands" Project.runCmd $ simpleParser Project.options
                   do addCommand "set" "set project configuration values" Project.set (strArgument (metavar "value" <> help "namespace=value name=value etc."))
                      addCommand "list" "list project configurations" (const Project.list) (pure ())
                 addCommand "init" "Create the config.yaml file including templates" Init.init Init.options                 
                 addCommand "templates" "list known templates" Template.templatesCmd Template.options                 
                 addCommand "push" "Push project to K8's" Push.push Push.options                 
            runCmd



main :: IO ()
main = do
  -- files <- getFiles [] ["tsconfig.json", "package.json", "package-lock.json", "app"]
  -- print files
  -- md5s <-  computeMd5s files
  -- mapM (\(p, h) -> putStrLn (printf "%s %s" h p)) md5s

  -- md5s' <- runFindMd5 [] ["tsconfig.json", "package.json", "package-lock.json", "app"]
  -- mapM (\x -> putStrLn (printf "%s %s" (D.name x) (md5 x))) md5s'



  -- config <- readConfig
  -- let project = head $ projects config
  -- env <- readEnvironment
  -- let service = serviceFromProject env project
  -- K.createService (namespace env) service
  -- let ss = statefulSetFromProject env project
  -- K.createStatefulSet (namespace env) ss




  -- Right res <- K.execCmd "odo" "nodejs-847f6f9bbb-lbwct" ["cat", "/etc/os-release"]
  -- putStrLn res

  -- res <- K.copy "odo" "nodejs-6c747c4d84-qrkpg" "/" [] "/project"
  -- res <- K.getPodsWithName "odo" "foobar"
  -- let names = do
  --               pods <- res
  --               return $ map K.podName pods

  -- let labels = do
  --               pods <- res
  --               return $ map K.podLabels pods

  -- print res
  -- print labels

  res <- parse
  case res of
    Right _ -> return ()
    Left err -> die err