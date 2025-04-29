module Math.Multinomial where 

import Data.Ratio (Rational)
import Math.Ring

type OVar = String
type OCoeff = Rational

instance Ring OCoeff where
  radd = (+)
  rmul = (*)
  rneg = negate
  rzero = 0
  rone = 1

-- Represents a normal form of a multivariable polynomial
data Multinomial = MCoeff OCoeff
  | MVar OVar [Multinomial]
  deriving (Show, Eq, Ord)

multinomialVars :: Multinomial -> [OVar]
multinomialVars (MCoeff _) = []
multinomialVars (MVar var rest) = var : multinomialVars (head rest)

extractCoef :: Multinomial -> Int -> Multinomial
extractCoef (MVar _ coeffs) i = coeffs !! i

degVar :: OVar -> Multinomial -> Int
degVar v (MCoeff _) = 0
degVar v (MVar v' coefs) | v == v'   = length coefs
                         | otherwise = degVar v (head coefs) 