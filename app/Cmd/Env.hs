module Cmd.Env (set) where

import Environment (updateEnvironment)
import qualified Environment as Env
import Text.Printf (printf)
import qualified Kubernetes as K


set :: String -> IO (Either String ())
set v = case name of
  "namespace" -> do 
    -- check namespace exists
    namespaces <- K.listNamespaces
    case namespaces of 
      Left v -> return $ Left v
      Right ns -> 
        if (elem value (map K.namespaceName ns)) then do
          updateEnvironment (\cfg -> cfg { Env.namespace = value })
          putStrLn $ "namespace set to " ++ value
          return $ Right ()
        else
          return $ Left $ printf "Namespace %s does not exist" value
  v -> return $ Left $ printf "Unknown variable %s" v
  where
    (name, value') = span (\x -> x /= '=') v 
    value = tail value'
