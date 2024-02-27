module MyLib (startServer) where

-- import Data.ByteString.Lazy.Char8 as C

import Data.Aeson (decode)
import Featurizer (features)
import Network.HTTP.Types.Status (status400)
import Track
  ( Track (..),
    TrackError (..),
  )
import Web.Scotty

-- ActionM is a monad that has a returning value
-- usually I think will not have any return value at all
helloAction :: ActionM ()
helloAction = do
  requestData <- body
  -- liftIO $ C.putStrLn requestData
  case decode requestData :: Maybe Track of
    Nothing -> do
      _ <- status status400
      json TrackError {trackErrorName = "error on decode data"}
    Just track -> do
      let feats = features track
      json feats

routes :: ScottyM ()
routes = do
  post "/track" helloAction

startServer :: IO ()
startServer = scotty 3000 routes
