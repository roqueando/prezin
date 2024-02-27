module OneHotEncoder (OneHotEncoder (..), fit, transform) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import GHC.Generics

data OneHotEncoder = OHE
  { -- mapping deve conter todos os valores de 0 até N
    -- onde N é o tamanho de mapping (length mapping)
    mapping :: M.Map T.Text Int,
    numFeatures :: Int
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

type VectorString = [T.Text]

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

-- | will return the mapping necessary to transform
fit :: VectorString -> M.Map T.Text Int
fit vs =
  if null vs
    then undefined
    else M.fromList newMapping
  where
    newMapping = [(x, index) | (index, x) <- enumerate vs]

-- | will transform in a 2D vector with 1 in the right position that you want
transform :: OneHotEncoder -> T.Text -> [Float]
transform ohe str =
  if M.size m == 0
    then []
    else transformInplace feats str
  where
    m = mapping ohe
    nf = numFeatures ohe
    feats = replicate nf 0.0
    transformInplace :: [Float] -> T.Text -> [Float]
    transformInplace f v =
      if length f /= nf
        then []
        else case M.lookup v m of
          Nothing -> []
          Just index -> [if i == index then 1.0 else 0.0 | (i, _) <- enumerate feats]
