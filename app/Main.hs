module Main where

import MyLib (startServer)

main :: IO ()
main = do
  putStrLn "starting server..."
  startServer
