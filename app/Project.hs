module Project (Project(..)) where

import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics

data Project = Project { name :: !String
                       , template :: !String
                       } deriving (Show, Generic)

instance FromJSON Project
instance ToJSON Project
