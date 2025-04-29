module Main where

import Logarithmical.ONotation

main :: IO ()
main = do
  parameterName <- getLine
  functionName <- getLine
  raw_cfg <- getLine
  let cfg = read raw_cfg :: SCFG
  putStrLn $ resolveEquation parameterName $ calculateAsymptotics functionName cfg
