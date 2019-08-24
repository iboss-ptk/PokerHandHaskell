module Main where

import PokerHand

main :: IO ()
main = do
  line <- getLine
  putStrLn $ judge line
