module Template (Template(..)) where

import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics

data Template = Template { name :: !String
                         , buildContainer :: !String
                         , deployContainer :: !String
                         } deriving (Show, Generic)

instance FromJSON Template
instance ToJSON Template
