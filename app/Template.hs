module Template (Template(..)) where

import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics

data Template = Template { name :: !String
                         , buildContainer :: !String
                         , deployContainer :: !String
                         , ports :: ![Int]
                         } deriving (Show, Generic)

instance FromJSON Template
instance ToJSON Template
