module Track
  (Track (..),
    TrackResource (..),
    TrackError (..)
  ) where

import Data.Aeson
  (FromJSON,
    ToJSON,
    toJSON,
    parseJSON,
    genericToJSON,
    genericParseJSON
    )

import qualified Data.Text as T
import qualified Data.Map as M
import GHC.Generics
import Data.Aeson.Casing (aesonPrefix, snakeCase)

data Track = Track
  {
    trackId :: !T.Text,
    trackResource :: !TrackResource,
    trackTarget :: !TrackResource,
    trackAttributes :: M.Map T.Text T.Text
  } deriving (Eq, Show, Generic)

instance ToJSON Track where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON Track where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data TrackResource = TrackResource
  {
    trackResourceName :: !T.Text,
    trackResourceAction :: !T.Text
  } deriving (Eq, Show, Generic)

instance ToJSON TrackResource where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON TrackResource where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase


newtype TrackError = TrackError
  {
    trackErrorName :: T.Text
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

