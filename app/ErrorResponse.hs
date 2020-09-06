module ErrorResponse(ErrorResponse(..)) where

import Data.Aeson.Types       (toJSON, fromJSON, FromJSON, parseJSON)
import Data.Aeson             (encode, eitherDecode)
import GHC.Generics

data ErrorResponse = ErrorResponse  { message :: !String } deriving (Show, Generic)
instance FromJSON ErrorResponse
