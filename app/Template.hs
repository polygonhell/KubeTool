module Template (Template(..)) where

import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics

data Template = Template { name :: !String
                         , buildContainer :: !String
                         , deployContainer :: !String
                         , ports :: ![Int]
                         , sourceFiles :: ![String]
                         , runDevCommand :: !String
                         } deriving (Show, Generic)

instance FromJSON Template
instance ToJSON Template
