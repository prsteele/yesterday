module Main where

import Yesterday

main :: IO ()
main = do
  _ <- parseDirectory "TODO"
  pure ()
