module Main where

import Server.App (run)

port :: Int
port = 5000

main :: IO ()
main = do
  run port
