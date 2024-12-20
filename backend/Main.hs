module Main where

import TotalNormalization.ONotation

main :: IO ()
main = do 
  value <- readLn 
  putStr $ "O(" ++ showAsymptotics (calculateAsymptotics value) "n" ++ ")\n"
