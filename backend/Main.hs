module Main where

import Polynomial.ONotation

main :: IO ()
main = do 
  raw_cfg <- getLine
  let cfg = read raw_cfg :: SCFG
  putStrLn $ printAsymptotics $ oNormalForm "n" $ calculateAsymptotics cfg
