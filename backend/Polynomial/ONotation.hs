{-# LANGUAGE FlexibleInstances #-}

module Polynomial.ONotation where 

import Data.Ratio
import qualified Data.Set as Set

type OVar = String
type OCoeff = Rational


-- Represents ring with the identity element for multiplication 
class Eq d => Ring d where
   radd :: d -> d -> d
   rmul :: d -> d -> d
   rneg :: d -> d
   rsub :: d -> d -> d 
   rsub a b = a `radd` rneg b
   rzero :: d
   rone :: d

data OExpr = OVar OVar
  | OCoeff OCoeff
  | OSum OExpr OExpr
  | OProd OExpr OExpr
  | OCounter OVar OExpr OExpr OExpr
   deriving (Eq, Show, Read)

constPropagation :: OExpr -> OExpr
constPropagation v@(OVar _) = v
constPropagation c@(OCoeff _) = c
constPropagation (OSum lhs rhs) = case (constPropagation lhs, constPropagation rhs) of 
  ((OCoeff l), (OCoeff r)) -> OCoeff $ l + r
  ((OCoeff 0), r)          -> r
  (l, (OCoeff 0))          -> l
  (l, r)                   -> OSum l r
constPropagation (OProd lhs rhs) = case (constPropagation lhs, constPropagation rhs) of 
  ((OCoeff l), (OCoeff r)) -> OCoeff $ l * r
  ((OCoeff 1), r)          -> r
  (l, (OCoeff 1))          -> l
  (l, r)                   -> OProd l r 
constPropagation (OCounter c f t e) = OCounter c (constPropagation f) (constPropagation t) (constPropagation e)

prettyExpr :: OExpr -> String
prettyExpr (OCoeff n) = if denominator n /= 1 then 
    (show $ fromIntegral (numerator n) / fromIntegral (denominator n))
  else 
    show $ numerator n
prettyExpr (OVar var) = var 
prettyExpr (OSum lhs rhs) = "(" ++ prettyExpr lhs ++ " + " ++ prettyExpr rhs ++ ")"
prettyExpr (OProd lhs rhs) = "(" ++ prettyExpr lhs ++ " * " ++ prettyExpr rhs ++ ")"
prettyExpr (OCounter c f t e) = "(sum from " ++ c ++ " = " ++ prettyExpr f ++ " to " ++ prettyExpr t ++ " of " ++ prettyExpr e ++ ")"

freeVars :: OExpr -> Set.Set OVar
freeVars (OCoeff _) = Set.empty
freeVars (OVar name) = Set.singleton name
freeVars (OProd lhs rhs) = freeVars lhs `Set.union` freeVars rhs
freeVars (OSum lhs rhs) = freeVars lhs `Set.union` freeVars rhs
freeVars (OCounter counter from to expr) = counter `Set.delete` (freeVars expr `Set.union` freeVars from `Set.union` freeVars to)

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

instance Ring OCoeff where 
  radd = (+)
  rmul = (*)
  rneg = negate
  rzero = 0
  rone = 1

removeLeadingZeroes :: Ring d => [d] -> [d]
removeLeadingZeroes vals = let raw = reverse $ dropWhile (== rzero) $ reverse vals in
  if null raw then [rzero] else raw

addPoly :: Ring d => [d] -> [d] -> [d]
addPoly a b = removeLeadingZeroes $ fmap (uncurry radd) $ zipPadded a b 
  where 
    zipPadded [] ys = zip (repeat $ rzero) ys
    zipPadded xs [] = zip xs (repeat $ rzero)
    zipPadded (x:xs) (y:ys) = (x, y) : zipPadded xs ys

prodPoly :: Ring d => [d] -> [d] -> [d]  
prodPoly [] _ = [rzero]
prodPoly (x : xs) ys = addPoly (rzero : (prodPoly xs ys)) (fmap (`rmul` x) ys)

-- TODO: use Faulhabers formula directly
sumOfPowers :: [[OExpr]]
sumOfPowers = fmap (fmap OCoeff) $ [
  [0, 1], 
  [0, 1 % 2, 1 % 2],
  [0, 1 % 6, 1 % 2, 1 % 3], 
  [0, 0, 1 % 4, 1 % 2, 1 % 4]]

rsumMany :: Ring d => [d] -> d
rsumMany [] = rzero
rsumMany arr = foldr1 radd arr

symbolicPower :: Ring d => Int -> d -> d
symbolicPower 0 _ = rone
symbolicPower n expr = foldr1 rmul $ take n $ repeat expr

substPoly :: Ring d => [d] -> d -> d 
substPoly coefs val = foldr1 radd $ fmap (\ (i, v) -> v `rmul` symbolicPower i val) $ enumerate coefs
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
oSemiNormalForm var (OCounter counter from to expr) = let normalizedExpr = oSemiNormalForm counter expr in 
  let n = length normalizedExpr - 1 in 
    oSemiNormalForm var $ rsumMany [bJ `rmul` (substPoly pJ to `rsub` substPoly pJ from) | 
      j <- [0..n],
      let pJ = sumOfPowers !! j, 
      let bJ = normalizedExpr !! j]

data SCFG  = 
    SAtom                                  -- O(1) operations, such as arithmetics 
  | SSkip                                  -- noop
  | SIf SCFG SCFG                          -- if (A) {then} {else} 
  | SCounterFor OVar OExpr OExpr SCFG      -- for (i = A; i < B; i++) { body }
  | SBlock [SCFG]                          -- basic block of CFG 
  deriving (Eq, Show, Read)

-- Represents a normal form of a multivariable polynomial
data Multinomial = MCoeff OCoeff
  | MVar OVar [Multinomial]
  deriving (Show, Eq)

oNormalForm :: OVar -> OExpr -> Multinomial
oNormalForm name expr = oNormalFormImpl exprVars expr' where 
  expr' = constPropagation expr
  exprVars :: [OVar]
  exprVars = name : (Set.elems $ Set.delete name (freeVars expr'))
  oNormalFormImpl :: [OVar] -> OExpr -> Multinomial
  oNormalFormImpl  [] (OCoeff v) = MCoeff v
  oNormalFormImpl  [] _ = undefined -- TODO: throw/catch  
  oNormalFormImpl (var:vars) expr = MVar var $ fmap (oNormalFormImpl vars) (fmap constPropagation $ oSemiNormalForm var expr) 

multinomialVars :: Multinomial -> [OVar]
multinomialVars (MCoeff _) = []
multinomialVars (MVar var rest) = var : (multinomialVars $ head rest)

multinomialExpr :: Multinomial -> OExpr
multinomialExpr (MCoeff coef) = OCoeff coef
multinomialExpr (MVar var coefs) = substPoly (fmap multinomialExpr coefs) (OVar var)

printAsymptotics :: Multinomial -> String 
printAsymptotics (MVar _ coefs ) = let n = length coefs -1 in 
  case  multinomialExpr $ coefs !! n of 
    (OCoeff _) ->  "O(n ^ " ++ show n ++ ")"
    v          ->  "O(" ++ prettyExpr v ++ " * n ^ " ++ show n ++ ")" 

-- Receives the program CFG, reduces it to receivce a complex expression, which describes a number of operations it performs  
calculateAsymptotics :: SCFG -> OExpr 
calculateAsymptotics SAtom = OCoeff 1
calculateAsymptotics SSkip = OCoeff 0
calculateAsymptotics (SIf t e) = OSum (calculateAsymptotics t) (calculateAsymptotics e) -- TODO: add MAX
calculateAsymptotics (SCounterFor counter from to body) = OCounter counter from to $ calculateAsymptotics body
calculateAsymptotics (SBlock exprs) = rsumMany $ fmap calculateAsymptotics exprs







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
