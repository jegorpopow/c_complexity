module Main where

import Data.Map qualified as Map
import Data.Maybe (Maybe (Just, Nothing), isJust)
import Logarithmical.OExpr
import Logarithmical.ONotation
import Math.Multinomial
import Text.Read (readMaybe)

parseFunctionContext :: [String] -> Maybe CFGContext
parseFunctionContext strs
  | length strs `mod` 3 /= 0 = Nothing
  | otherwise = Map.fromList <$> mapM parseTriple (splitTriples strs)
  where
    splitTriples :: [a] -> [(a, a, a)]
    splitTriples [] = []
    splitTriples (a : b : c : rest) = (a, b, c) : splitTriples rest
    parseTriple :: (String, String, String) -> Maybe (OVar, (OVar, SCFG))
    parseTriple (a, b, c) = (a,) . (b,) <$> (readMaybe c :: Maybe SCFG)

main :: IO ()
main = do
  lns <- lines <$> getContents
  if null lns
    then
      putStrLn "Backend received empty input"
    else case lns of
      targetName : functionDescs ->
        case parseFunctionContext functionDescs of
          Nothing -> putStrLn "Can not parse function descriptions"
          (Just cfgsCtx) -> do
            if not $ Map.member targetName cfgsCtx
              then
                putStrLn "No target function found among provided descriptions"
              else
                let ctx = inlineInContext targetName . getFunctionContext $ cfgsCtx
                 in putStrLn $ "O(" ++ resolveEquation ctx targetName ++ ")"
