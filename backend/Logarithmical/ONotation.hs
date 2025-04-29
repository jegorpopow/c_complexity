{-# LANGUAGE FlexibleInstances #-}

module Logarithmical.ONotation where

import Data.Ratio ( (%), denominator, numerator )
import qualified Data.Set as Set
import Math.Faulhaber (faulhabersCoefs)
import Data.List (nub, null)
import Debug.Trace (trace)
import Math.Ring
import Math.Multinomial


data OExpr = OVar OVar
  | OCoeff OCoeff
  | OSum OExpr OExpr
  | OProd OExpr OExpr
  | OCounter OVar OExpr OExpr OExpr
  | ORecursive OExpr
   deriving (Eq, Show, Read)

substExpr :: OVar -> OExpr -> OExpr -> OExpr
substExpr name v v'@(OVar name') | name == name' = v
                                 | otherwise = v'
substExpr _ v v'@(OCoeff _) = v'
substExpr n v (OSum l r) = OSum (substExpr n v l) (substExpr n v r)
substExpr n v (OProd l r) = OProd (substExpr n v l) (substExpr n v r)
substExpr n v (OCounter c f t e) = OCounter c (substExpr n v f) (substExpr n v t) (substExpr n v e)
substExpr n v (ORecursive e) = ORecursive $ substExpr n v e

substRecursive :: OExpr -> OExpr -> OExpr
substRecursive v v'@(OVar name') = v'
substRecursive v v'@(OCoeff _) = v'
substRecursive v (OSum l r) = OSum (substRecursive v l) (substRecursive v r)
substRecursive v (OProd l r) = OProd (substRecursive v l) (substRecursive v r)
substRecursive v (OCounter c f t e) = OCounter c f t (substRecursive v e)
substRecursive v (ORecursive e) = v

constFolding :: OExpr -> OExpr
constFolding v@(OVar _) = v
constFolding c@(OCoeff _) = c
constFolding (OSum lhs rhs) = case (constFolding lhs, constFolding rhs) of
  (OCoeff l, OCoeff r) -> OCoeff $ l + r
  (OCoeff 0, r)          -> r
  (l, OCoeff 0)          -> l
  (l, r)                   -> OSum l r
constFolding (OProd lhs rhs) = case (constFolding lhs, constFolding rhs) of
  (OCoeff l, OCoeff r) -> OCoeff $ l * r
  (OCoeff 1, r)          -> r
  (l, OCoeff 1)          -> l
  (l, r)                   -> OProd l r
constFolding (OCounter c f t e) = OCounter c (constFolding f) (constFolding t) (constFolding e)
constFolding (ORecursive e) = ORecursive $ constFolding e

prettyExpr :: OExpr -> String
prettyExpr (OCoeff n) = if denominator n /= 1 then
    show $ fromIntegral (numerator n) / fromIntegral (denominator n)
  else
    show $ numerator n
prettyExpr (OVar var) = var
prettyExpr (OSum lhs rhs) = "(" ++ prettyExpr lhs ++ " + " ++ prettyExpr rhs ++ ")"
prettyExpr (OProd lhs rhs) = "(" ++ prettyExpr lhs ++ " * " ++ prettyExpr rhs ++ ")"
prettyExpr (OCounter c f t e) = "(sum from " ++ c ++ " = " ++ prettyExpr f ++ " to " ++ prettyExpr t ++ " of " ++ prettyExpr e ++ ")"
prettyExpr (ORecursive e) = "f(" ++ prettyExpr e ++ ")"

recursiveCalls :: OVar -> OExpr -> [Multinomial]
recursiveCalls _ (OCoeff _) = []
recursiveCalls _ (OVar name) = []
recursiveCalls v (OProd lhs rhs) = recursiveCalls v lhs ++ recursiveCalls v rhs
recursiveCalls v (OSum lhs rhs) = recursiveCalls v lhs ++ recursiveCalls v rhs
recursiveCalls v (OCounter counter from to expr) = recursiveCalls v expr
recursiveCalls v (ORecursive expr) = [oNormalForm v expr]

freeVars :: OExpr -> Set.Set OVar
freeVars (OCoeff _) = Set.empty
freeVars (OVar name) = Set.singleton name
freeVars (OProd lhs rhs) = freeVars lhs `Set.union` freeVars rhs
freeVars (OSum lhs rhs) = freeVars lhs `Set.union` freeVars rhs
freeVars (OCounter counter from to expr) = counter `Set.delete` (freeVars expr `Set.union` freeVars from `Set.union` freeVars to)
freeVars (ORecursive expr) = freeVars expr

instance Ring OExpr where
  -- Smart constructors for OSum
  radd (OCoeff 0) x = x
  radd x (OCoeff 0) = x
  radd (OCoeff x) (OCoeff y) = OCoeff $ x + y
  radd x y = OSum x y
  -- Smart constructors for OProd
  rmul (OCoeff 1) x = x
  rmul x (OCoeff 1) = x
  rmul (OCoeff x) (OCoeff y) = OCoeff $ x * y
  rmul x y = OProd x y
  rneg = (OCoeff (-1) `OProd`)
  rzero = OCoeff 0
  rone = OCoeff 1

substPoly :: Ring d => [d] -> d -> d
substPoly coefs val = foldr1 radd ((\ (i, v) -> v `rmul` symbolicPower i val) <$> enumerate coefs)
  where
    enumerate :: [a] -> [(Int, a)]
    enumerate lst = let n = length lst - 1 in zip [0..n] lst

-- receives a name of variable `n`, represents given expression 
-- as `A * n^0 + B * n^1 + C * n ^ 2 + ...`, where A, B, C a expressions, which depends on 
-- free variables of given expression only
oSemiNormalForm :: OVar -> OExpr -> [OExpr]
oSemiNormalForm _ v@(OCoeff _) = [v]
oSemiNormalForm var v@(OVar var') = if var' == var then [OCoeff 0, OCoeff 1] else [v]
oSemiNormalForm var (OProd lhs rhs) = prodPoly (oSemiNormalForm var lhs) (oSemiNormalForm var rhs)
oSemiNormalForm var (OSum lhs rhs) = addPoly (oSemiNormalForm var lhs) (oSemiNormalForm var rhs)
oSemiNormalForm var v@(ORecursive _) = [v]
oSemiNormalForm var (OCounter counter from to expr) = let normalizedExpr = oSemiNormalForm counter expr in
  let n = length normalizedExpr - 1 in
    oSemiNormalForm var $ rsumMany [bJ `rmul` (substPoly pJ to `rsub` substPoly pJ from) |
      j <- [0..n],
      let pJ = fmap OCoeff $ faulhabersCoefs !! j,
      let bJ = normalizedExpr !! j]

data SCFG  =
    SAtom                                  -- O(1) operations, such as arithmetics 
  | SSkip                                  -- noop
  | SCall OVar OExpr                       -- call to another function f with known parameter  
  | SHardcoded OExpr                       -- hardcoded element with known complexity (used for some variants)
  | SIf SCFG SCFG                          -- if (A) {then} {else} 
  | SCounterFor OVar OExpr OExpr SCFG      -- for (i = A; i < B; i++) { body }
  | SBlock [SCFG]                          -- basic block of CFG 
  deriving (Eq, Show, Read)


oNormalForm :: OVar -> OExpr -> Multinomial
oNormalForm name expr = oNormalFormImpl exprVars expr' where
  expr' = constFolding expr
  exprVars :: [OVar]
  exprVars = name : Set.elems (Set.delete name (freeVars expr'))
  oNormalFormImpl :: [OVar] -> OExpr -> Multinomial
  oNormalFormImpl  [] (OCoeff v) = MCoeff v
  oNormalFormImpl  [] _ = undefined -- TODO: throw/catch  
  oNormalFormImpl (var:vars) expr = MVar var $ fmap (oNormalFormImpl vars . constFolding) (oSemiNormalForm var expr)

multinomialExpr :: Multinomial -> OExpr
multinomialExpr (MCoeff coef) = OCoeff coef
multinomialExpr (MVar var coefs) = substPoly (fmap multinomialExpr coefs) (OVar var)

printAsymptotics :: OVar -> Multinomial ->  String
printAsymptotics name (MVar _ coefs) = let n = length coefs -1 in
  case  multinomialExpr $ coefs !! n of
    (OCoeff _) ->  "O(" ++ name ++" ^ " ++ show n ++ ")"
    v          ->  "O(" ++ prettyExpr v ++ " * " ++ name ++" ^ " ++ show n ++ ")"

-- Receives the program CFG, reduces it to receivce a complex expression, which describes a number of operations it performs  
calculateAsymptotics :: OVar -> SCFG -> OExpr
calculateAsymptotics _ SAtom = OCoeff 1
calculateAsymptotics _ SSkip = OCoeff 0
calculateAsymptotics name (SCall name' arg) | name == name' = ORecursive arg
                                            | otherwise     = undefined
calculateAsymptotics _ (SHardcoded expr) = expr
calculateAsymptotics n (SIf t e) = OSum (calculateAsymptotics n t) (calculateAsymptotics n e) -- This is pretty correct, but there are more accurate estimations 
calculateAsymptotics n (SCounterFor counter from to body) = OCounter counter from to $ calculateAsymptotics n body
calculateAsymptotics n (SBlock exprs) = rsumMany $ fmap (calculateAsymptotics n) exprs

masterTheorem :: OVar -> Multinomial -> Multinomial -> String
masterTheorem n arg body = case multinomialExpr (extractCoef arg 1) of 
  (OCoeff inv_b) -> let b = 1 / inv_b in 
    case multinomialExpr(extractCoef body 1) of 
      (OCoeff a) -> let f = oNormalForm n (multinomialExpr (extractCoef body 1)) in 
        let c = degVar n f in
          let b_pow_c = symbolicPower c b in 
            case compare a b_pow_c of 
              GT -> "O(" ++ n ++ " ^ (log_{" ++ show b ++ "}(" ++ show a ++ ")))"
              LT -> "O(" ++ n ++ " ^ (" ++ show c ++ "))"
              EQ -> "O(" ++ n ++ " ^ (" ++ show c ++ ") * log(" ++ n ++ "))" 
      _          -> "Unsupported recurence" 
  _          -> "Unsupported recurence"


resolveEquation :: OVar -> OExpr -> String
resolveEquation v e | null rc                 = printAsymptotics v (oNormalForm v e)
                    | length (nub rc) > 1     = undefined                -- TODO
                    | otherwise = let reccall = head rc in 
                      let multinomial = oNormalForm "%REC%" (substRecursive (OVar "%REC%") e) in 
                        masterTheorem v reccall multinomial
  where 
    traced :: Show a => a -> a
    traced x = trace (show x) x
    rc = recursiveCalls v e






------------- EXAMPLES ----------------

exBubbleSort :: SCFG
exBubbleSort = SBlock [
  SAtom, -- initialization 
  SCounterFor "i" (OCoeff 0) (OVar "n") ( -- for (int i = 0; i < n; i++)
    SCounterFor "j" (OCoeff 1) ((OVar "n" `rsub` OVar "i") `rsub` OCoeff 1) ( -- for (int j = 1; j < n - i - 1; j++)
      SIf SAtom SSkip -- if (a[j - 1] > a[j]) swap(a + i, a + j); 
    )
  ),
  SAtom] -- return 

exDivideAndRule :: SCFG
exDivideAndRule = SBlock [
  SAtom,
  SCounterFor "i" (OCoeff (0 % 1)) (OVar "n") (SBlock [SAtom]),
  SCall "divide_and_rule" (OProd (OCoeff (1 % 2)) (OVar "n")),
  SCall "divide_and_rule" (OProd (OCoeff (1 % 2)) (OVar "n")), 
  SAtom]