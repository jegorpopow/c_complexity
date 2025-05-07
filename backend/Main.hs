module Main where

import Logarithmical.ONotation
import qualified Data.Map as Map  
import Data.Maybe (isJust, Maybe (Nothing, Just))


pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (name:desc:rest) = (name, desc) : pairs rest  

main :: IO ()
main = do
  -- the target of the analysis
  parameterName <- getLine
  functionName <- getLine
  -- list of cfgs of functions from TU
  cfgsDesc <- getContents
  let cfgs = Map.fromList $ (\ (a, b) -> (a, read b:: SCFG)) <$> (pairs . lines $ cfgsDesc)
  let cfg = (Map.!?) cfgs functionName

  case cfg of 
    Nothing    -> putStrLn "Failed to found corresponding function"
    (Just cfg) -> putStrLn $ resolveEquation parameterName $ calculateAsymptotics functionName cfg

