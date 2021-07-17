{-# LANGUAGE OverloadedStrings #-}

module Cmd.Push where

import Config (Config (..), readConfig)
import Control.Concurrent (threadDelay)
import Control.Monad ( join, forever, void )
import Data.Either (fromRight)
import Data.List (find, intercalate, intersect, (\\), isPrefixOf)
import Data.Maybe (fromJust, isJust)
import Data.Text (unpack)
import Debug.Trace(trace)
import qualified Directory as D
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
import Cmd.Utils

import System.FSNotify
import System.FilePath ((</>), splitPath, splitDirectories)



data Options = Options
  { projectName :: Maybe String,
    optQuiet :: Bool
  }
  deriving (Show)

options :: Parser Options
options =
  Options <$> optional (strOption (long "name" <> short 'n' <> help "Name of the Project. Can be omitted if there is a single Project in the Configuration"))
    <*> flag False True (short 'b' <> long "foofoo")

pushDevContainer :: Environment -> Config -> Project -> Template -> IO (Either String String)
pushDevContainer env config proj template = do
  let ns = namespace env
  let name = P.name proj
  podsO <- getPodsWithName ns name
  -- TODO deal with multiple instances case
  -- putStrLn $ printf "podsO = %s" (show podsO)
  case podsO of
    Right [pod] -> do
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
      let serviceDef = serviceFromProject env proj template
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


filesToCopy :: Project -> Template -> (String, String) -> IO (Either String ([String], [String]))
filesToCopy _ t (ns, podName) = do
  -- get the local file list
  let srcs = T.sourceFiles t
  local <- D.runFindMd5 [] srcs
  fc <- D.findCommand "-exec md5sum {} \\;" [] srcs
  findRes <- execCmd ns podName ["bash", "-c", "cd /project && " ++ fc]
  let remote = fmap D.packageFindResult findRes
  let files = do
        r <- remote
        let toDelete = map D.name r \\ map D.name local
        let toCopy = map D.name $ local \\ (r `intersect` local)
        return (toDelete, toCopy)

  return files

-- collapse :: Either a (IO (Either a b)) -> IO (Either a b)
collapse :: (Traversable m, Monad m, Monad f) => m (f (m a)) -> f (m a)
collapse x = join <$> sequence x



updateFiles :: Environment -> Config -> Project -> Template -> IO (Either String (String, String))
updateFiles env config proj template = do
  putStrLn $ printf "Pushing project"
  let ns = namespace env
  baseDir <- getCurrentDirectory
  podName <- pushDevContainer env config proj template
  files <- collapse do
    pod <- podName
    return $ filesToCopy proj template (ns, pod)
  let dest = "/project"

  -- delete the files removed locally
  -- TODO deal with filenames with spaces etc
  deleted <- case files of
    Left _ -> return $ Right ()
    Right ([], _) -> return $ Right ()
    Right (toDelete, _) -> do
      putStrLn $ printf "deleting %d container files\n  %s" (length toDelete) (intercalate "\n  " toDelete)
      sequence do
        pod <- podName
        let cmd = printf "echo \"%s\" | xargs rm" $ unwords toDelete :: String
        return $ Control.Monad.void (execCmd ns pod ["bash", "-c", "cd /project && " ++ cmd])

  -- copy the changed files
  copied <- case files of
    Left _ -> return $ Right ()
    Right (_, []) -> return $ Right ()
    Right (_, toCopy) -> do
      putStrLn $ printf "copying changed files to container\n  %s" (intercalate "\n  " toCopy)
      sequence do
        pod <- podName
        return $ () <$ copy ns pod baseDir toCopy dest


  return $ do
    deleted
    copied
    name <- podName
    return (ns, name)



pushProject :: Environment -> Config -> Project -> Template -> IO (Either String String)
pushProject env config proj template = do
  details <- updateFiles env config proj template
  case details of
    Left err -> return $ Left err
    Right (ns, podName) ->
      let runCmd = printf "echo '%s' > cmd && supervisorctl -s unix:///tmp/supervisor.sock restart run" ("cd /project && " ++ T.pushCommand template) :: String in
        do
          ran <- execCmd ns podName ["bash", "-c", runCmd]
          print ran
          return ran


watchProject :: Environment -> Config -> Project -> Template -> IO (Either String String)
watchProject env config proj template = do
  details <- updateFiles env config proj template
  case details of
    Left err -> return $ Left err
    Right (ns, podName) ->
      if isJust(T.watchCommand template) then
        let runCmd = printf "echo '%s' > cmd && supervisorctl -s unix:///tmp/supervisor.sock restart run" ("cd /project && " ++ T.pushCommand template) :: String in
          do
            ran <- execCmd ns podName ["bash", "-c", runCmd]
            print ran
            return ran
      else do
        print "No Watch Command"
        return $ Right "No Watch Command"


push :: Options -> IO (Either String ())
push opt = withEnvironment $ \env ->
  withConfig $ \config -> do
    let ps' = case projectName opt of
          Nothing -> Right $ projects config
          Just n -> case find (\x -> P.name x == n) (projects config) of
            Nothing -> Left (printf "Bad Project Name %s" n :: String)
            Just p -> Right [p]

    case ps' of
      Left e -> return $ Left e
      Right ps -> do
        let ts = templates config
        let foo = mapM (\p -> fmap (\t -> (p, t)) (find (\t -> T.name t == P.template p) ts)) ps
        case mapM (\p -> fmap (\t -> (p, t)) (find (\t -> T.name t == P.template p) ts)) ps of
          Just xs -> do
            putStrLn $ printf "Pushing projects %s" $ intercalate ", " (map P.name ps)
            foo <- mapM (uncurry (pushProject env config)) xs
            return $ () <$ sequence foo
          Nothing -> return $ Left "Referenced template missing"


-- TODO actually watch the filesystem
watch :: Options -> IO (Either String ())
watch opt = withEnvironment $ \env ->
  withConfig $ \config ->
  withProject (projectName opt) $ \p -> do
    let ts = templates config
    let foo = find (\t -> T.name t == P.template p) ts
    case foo of
      Just t -> do
        putStrLn $ printf "Pushing project %s" $ P.name p
        -- push the Project
        pushProject env config p t
        -- Then watch the filesystem
        watchFS env config p t
      Nothing -> return $ Left "Referenced template missing"




watchPredicate :: FilePath -> Template -> Event -> Bool
watchPredicate cwd tp ev = included where
  files = map (\s -> splitDirectories (cwd </> s)) $ T.sourceFiles tp
  fileName = splitDirectories $ eventPath ev
  test = map (`isPrefixOf` fileName) files
  included = any (`isPrefixOf` fileName) files


watchAction :: Environment -> Config -> Project -> Template -> Event -> IO ()
watchAction env c p t ev = do
  print $ eventPath ev
  watchProject env c p t
  return ()

watchFS :: Environment -> Config -> Project -> Template -> IO (Either String ())
watchFS env c p t = withManager \mgr -> do
  config <- readConfig
  cwd <- getCurrentDirectory
  watchTree mgr "." (watchPredicate cwd t) (watchAction env c p t)
  forever $ threadDelay 1000000
