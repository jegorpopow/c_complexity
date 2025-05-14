{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Logarithmical.ONotation where

import Data.List (isPrefixOf, nub, null)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, Maybe (Just, Nothing))
import Data.Ratio (denominator, numerator, (%))
import Data.Set qualified as Set
import Debug.Trace (trace)
import Distribution.Compat.Prelude (undefined)
import Logarithmical.OExpr
import Math.Faulhaber (faulhabersCoefs)
import Math.Multinomial
import Math.Ring

data SCFG
  = SAtom -- O(1) operations, such as arithmetics
  | SSkip -- noop
  | SCall OVar OExpr -- call to another function f with known parameter
  | SHardcoded OExpr -- hardcoded element with known complexity (used for some variants)
  | SIf SCFG SCFG -- if (A) {then} {else}
  | SCounterFor OVar OExpr OExpr OCounterVariant SCFG -- for (i = A; i < B; i++) { body }
  | SBlock [SCFG] -- basic block of CFG
  deriving (Eq, Show, Read)

type CFGContext = Map.Map OVar (OVar, SCFG)

singletonCfgCtx :: OVar -> OVar -> SCFG -> CFGContext
singletonCfgCtx fname aname body = Map.singleton fname (aname, body)

-- Receives the program CFG, reduces it to receivce a complex expression, which describes a number of operations it performs
calculateAsymptotics :: OVar -> SCFG -> OExpr
calculateAsymptotics _ SAtom = OCoeff 1
calculateAsymptotics _ SSkip = OCoeff 0
calculateAsymptotics _ (SCall name arg) = OCall name arg
calculateAsymptotics _ (SHardcoded expr) = expr
calculateAsymptotics n (SIf t e) = OSum (calculateAsymptotics n t) (calculateAsymptotics n e) -- This is pretty correct, but there are more accurate estimations
calculateAsymptotics n (SCounterFor counter from to i body) = OCounter counter from to i $ calculateAsymptotics n body
calculateAsymptotics n (SBlock exprs) = rsumMany $ fmap (calculateAsymptotics n) exprs

getFunctionContext :: CFGContext -> FunctionContext
getFunctionContext = Map.mapWithKey (\k (a, b) -> (a, calculateAsymptotics k b))

masterTheorem :: OVar -> OVar -> OExpr -> Maybe String
masterTheorem n fname body =
  let (dryed, bm) = extractCalls body
   in if not $ isMultinomial dryed
        then
          Nothing
        else case commonArgument n bm of
          Just arg ->
            let body = exprToMultinomial unifiedCallStr $ unifyCalls dryed
             in case multinomialToExpr (extractCoef arg 1) of
                  (OCoeff inv_b) ->
                    let b = 1 / inv_b
                     in ( case multinomialToExpr (extractCoef body 1) of
                            (OCoeff a) ->
                              ( let f = exprToMultinomial n (multinomialToExpr (extractCoef body 0))
                                 in let c = degVar n f
                                     in ( let b_pow_c = ringPower c b
                                           in ( Just $
                                                  case compare a b_pow_c of
                                                    GT -> n ++ " ^ log_{" ++ prettyRational b ++ "}" ++ prettyRational a
                                                    LT -> printDegree n c
                                                    EQ -> printDegree n c ++ " * " ++ prettyExpr (OLog (OVar n))
                                              )
                                        )
                              )
                            _ -> Nothing
                        )
                  _ -> Nothing
          Nothing -> Nothing
  where
    unifiedCallStr = "%%UNFIED_CALL"

    commonArgument :: OVar -> Bimap OVar OExpr -> Maybe Multinomial
    commonArgument n (l, r)
      | length (nub multinmialArgs) == 1 = Just $ head multinmialArgs
      | otherwise = Nothing
      where
        multinmialArgs = (\(OCall _ e) -> exprToMultinomial n e) <$> Map.keys r

    isCallingVar :: OExpr -> Bool
    isCallingVar (OVar name) = "%%CALL_" `isPrefixOf` name
    isCallingVar _ = False

    unifyCalls = substExpr isCallingVar (const $ OVar unifiedCallStr)

tailRecursion :: FunctionContext -> OVar -> OVar -> OExpr -> Maybe String
tailRecursion ctx n fname body =
  let (dryed, bm) = extractCalls body
   in case isTailRecursive dryed bm of
        Just (i, e) ->
          Just $ resolveEquation
            (Map.insert fname (n, OCounter "%%fresh" (OCoeff 0) (OVar n) i (substVar e n "%%fresh")) ctx)
            fname
        _  -> Nothing
  where
    substVar :: OExpr -> OVar -> OVar -> OExpr
    substVar s n n' =
      substExpr
        ( \case
            e@(OVar n'') -> n == n''
            _ -> False
        )
        (const (OVar n'))
        s

    isTailRecursive :: OExpr -> Bimap OVar OExpr -> Maybe (OCounterVariant, OExpr)
    isTailRecursive e (l, r) =
      if Map.size r /= 1
        then Nothing
        else
          let m = exprToMultinomial n arg
           in if (degVar n m /= 1) || multinomialToExpr (extractCoef m 1) /= OCoeff 1
                then
                  Nothing
                else
                  Just (OIncrenmentCounter, multinomialToExpr (extractCoef m 0))
      where
        (OCall _ arg) = head $ Map.keys r
        callVar = head $ (Map.!) r (head $ Map.keys r)

prettyRational :: OCoeff -> String
prettyRational = prettyExpr . OCoeff

printDegree :: OVar -> Int -> String
printDegree var n
  | n == 0 = "1"
  | n == 1 = var
  | otherwise = var ++ "^" ++ show n

resolveEquation :: FunctionContext -> OVar -> String
resolveEquation rawCtx name
  | isNonCalling ctx name && isMultinomial body =
      let (coef, degree) = getGreaterDegree (exprToMultinomial argName body)
       in case coef of
            (OCoeff _) -> printDegree argName degree
            _ -> prettyExpr coef ++ " * " ++ printDegree argName degree
  | isNonCalling ctx name && not (isMultinomial body) = "<unknown>"
  | isSimpleRecursive ctx name = case masterTheorem argName name body of
      (Just x) -> x
      _ -> fromMaybe "<unknown>" $ tailRecursion ctx argName name body
  | isMutualrecursive ctx name = "<unknown>"
  | otherwise = "<unknown>"
  where
    ctx = inlineInContext name rawCtx
    (argName, body) = lookupCtx ctx name
    getGreaterDegree :: Multinomial -> (OExpr, Int)
    getGreaterDegree (MCoeff c) = (OCoeff c, 0)
    getGreaterDegree (MVar _ coeffs) =
      let n = length coeffs - 1
       in (multinomialToExpr $ coeffs !! n, n)

------------- EXAMPLES ----------------

exBubbleSort :: SCFG
exBubbleSort =
  SBlock
    [ SAtom, -- initialization
      SCounterFor
        "i"
        (OCoeff 0)
        (OVar "n")
        OIncrenmentCounter
        ( -- for (int i = 0; i < n; i++)
          SCounterFor
            "j"
            (OCoeff 1)
            ((OVar "n" `rsub` OVar "i") `rsub` OCoeff 1)
            OIncrenmentCounter
            ( -- for (int j = 1; j < n - i - 1; j++)
              SIf SAtom SSkip -- if (a[j - 1] > a[j]) swap(a + i, a + j);
            )
        ),
      SAtom -- return
    ]

exDivideAndRule :: SCFG
exDivideAndRule =
  SBlock
    [ SAtom,
      SCounterFor "i" (OCoeff (0 % 1)) (OVar "n") OIncrenmentCounter (SBlock [SAtom]),
      SCall "divide_and_rule" (OProd (OCoeff (1 % 2)) (OVar "n")),
      SCall "divide_and_rule" (OProd (OCoeff (1 % 2)) (OVar "n")),
      SAtom
    ]