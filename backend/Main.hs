module Main where

import Polynomial.ONotation

main :: IO ()
main = do 
  parameterName <- getLine
  raw_cfg <- getLine
  let cfg = read raw_cfg :: SCFG
  putStrLn $ printAsymptotics parameterName $ oNormalForm parameterName $ calculateAsymptotics cfg
