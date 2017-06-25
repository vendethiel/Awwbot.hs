module Main where

import Disco (run)
import System.Environment (getEnv)

main :: IO ()
main = do
  token <- getEnv "DISCORD_AWW_TOKEN"
  run token