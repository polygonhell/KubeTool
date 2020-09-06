module Cmd.Config (set) where

import Config (updateConfig)
import qualified Config as Cfg
import Text.Printf (printf)


set :: String -> IO (Either String ())
set v = case name of
  "name" -> do
    updateConfig (\cfg -> cfg { Cfg.name = value })
    putStrLn $ "name set to " ++ value
    return $ Right ()
  "author" -> do
    updateConfig (\cfg -> cfg { Cfg.author = if value == "" then Nothing else Just value })
    putStrLn $ "author set to " ++ value
    return $ Right ()
  n -> return $ Left $ printf "Unknown variable %s" n
  where
    (name, value') = span (\x -> x /= '=') v 
    value = tail value'