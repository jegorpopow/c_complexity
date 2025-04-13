{-# LANGUAGE FlexibleInstances #-}

module V2.ONotation where 

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

freeVars :: OExpr -> Set.Set OVar
freeVars (OCoeff _) = Set.empty
freeVars (OVar name) = Set.singleton name
freeVars (OProd lhs rhs) = freeVars lhs `Set.union` freeVars rhs
freeVars (OSum lhs rhs) = freeVars lhs `Set.union` freeVars rhs
freeVars (OCounter counter from to expr) = undefined

instance Ring OExpr where
  radd (OCoeff 0) x = x
  radd x (OCoeff 0) = x
  radd (OCoeff x) (OCoeff y) = OCoeff $ x + y
  radd x y = OSum x y
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
oSemiNormalForm :: OExpr -> OVar -> [OExpr]  
oSemiNormalForm v@(OCoeff _) _ = [v]
oSemiNormalForm v@(OVar var') var = if var' == var then [OCoeff 0, OCoeff 1] else [v]
oSemiNormalForm (OProd lhs rhs) var = prodPoly (oSemiNormalForm lhs var) (oSemiNormalForm rhs var)
oSemiNormalForm (OSum lhs rhs) var = addPoly (oSemiNormalForm lhs var) (oSemiNormalForm rhs var)
oSemiNormalForm (OCounter counter from to expr) var = let normalizedExpr = oSemiNormalForm expr counter in 
  let n = length normalizedExpr - 1 in 
    (`oSemiNormalForm` var) $ rsumMany [bJ `rmul` (substPoly pJ to `rsub` substPoly pJ from) | 
      j <- [0..n],
      let pJ = sumOfPowers !! j, 
      let bJ = normalizedExpr !! j]

-- calculates the nf of polynom (or logarithmic polynom) in form [1, var, var^2, ...] 
-- oNormalForm :: OExpr -> OVar -> [OCoeff] 
-- oNormalForm (OCoeff val) _ = [val]
-- oNormalForm (OVar var') var = if var' == var then  [0, 1] else [0]
-- oNormalForm (OProd lhs rhs) var = prodONfs (oNormalForm lhs var) (oNormalForm rhs var)
-- oNormalForm (OSum lhs rhs) var = addONfs (oNormalForm lhs var) (oNormalForm rhs var) 
-- oNormalForm (OMax lhs rhs) var = maxONfs (oNormalForm lhs var) (oNormalForm rhs var)
-- oNormalForm (OLog _) _ = undefined

-- showAsymptotics ::  OExpr -> OVar -> String
-- showAsymptotics expr var = var ++ "^" ++ show (length (oNormalForm expr var) - 1)

data SCFG  = 
    SAtom                                  -- O(1) operations inside basic block, such as arithmetics 
  | SSkip                                  -- noop
  | SIf SCFG SCFG                          -- if (A) {then} {else} 
  | SCounterFor OVar OExpr OExpr SCFG      -- for (i = A; i < B; i++) { body }
  | SBlock [SCFG]                          -- basic block of CFG 
  deriving (Eq, Show, Read)


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
