module Featurizer (features) where

import Data.List (genericLength)
import qualified Data.Map.Strict as M
import OneHotEncoder (OneHotEncoder (..), fit, transform)
import Track (Track (..))

features :: Track -> [[Float]]
features track = map transformFunction mapKeys
  where
    transformFunction = transform ohe
    newMapping = fit mapKeys
    mapKeys = M.keys (trackAttributes track)
    ohe = OHE {mapping = newMapping, numFeatures = M.size newMapping}
