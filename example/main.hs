module Main where

import           App  (app)
import           App2 (test2)

main :: IO ()
main =
  test2 >> app
