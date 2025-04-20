module ONotation where

type OVar = String
type OCoeff = Int

data OExpr = OVar OVar
  | OSum OExpr OExpr
  | OProd OExpr OExpr
  | OCoeff OCoeff
  | OLog OExpr
  | OMax OExpr OExpr
  deriving (Eq, Show, Read)

addONfs :: [OCoeff] -> [OCoeff] -> [OCoeff]
addONfs a b = removeLeadingZeroes (uncurry (+) <$> zipPadded a b)
  where
    zipPadded [] ys = map (0,) ys
    zipPadded xs [] = map (, 0) xs
    zipPadded (x:xs) (y:ys) = (x, y) : zipPadded xs ys
    removeLeadingZeroes vals = let raw = reverse $  dropWhile (== 0) $ reverse vals  in if null raw then [0] else raw

prodONfs :: [OCoeff] -> [OCoeff] -> [OCoeff]
prodONfs [] _ = [0]
prodONfs (x : xs) ys = addONfs (0 : prodONfs xs ys) (fmap (* x) ys)

maxONfs :: [OCoeff] -> [OCoeff] -> [OCoeff]
maxONfs xs ys
  | length xs > length ys = xs
  | length ys > length xs = ys
  | ys !! (length ys - 1) > xs !! (length xs - 1) = ys
  | otherwise = xs

-- calculates the nf of polynom (or logarithmic polynom) in form [1, var, var^2, ...] 
oNormalForm :: OExpr -> OVar -> [OCoeff]
oNormalForm (OCoeff val) _ = [val]
oNormalForm (OVar var') var = if var' == var then  [0, 1] else [0]
oNormalForm (OProd lhs rhs) var = prodONfs (oNormalForm lhs var) (oNormalForm rhs var)
oNormalForm (OSum lhs rhs) var = addONfs (oNormalForm lhs var) (oNormalForm rhs var)
oNormalForm (OMax lhs rhs) var = maxONfs (oNormalForm lhs var) (oNormalForm rhs var)
oNormalForm (OLog _) _ = undefined

showAsymptotics ::  OExpr -> OVar -> String
showAsymptotics expr var = var ++ "^" ++ show (length (oNormalForm expr var) - 1)

-- TODO: specify expr and parse For cycles based on it

data SillyAST =
    SExpr                           -- Just some arithmetics 
  | SSkip
  | SIf SillyAST SillyAST           -- if (...) {then} {else} 
  | SCounterFor OVar OVar SillyAST  -- for i in 0:n  {smth}
  | SBlock [SillyAST]
  deriving (Eq, Show, Read)

calculateAsymptotics :: SillyAST -> OExpr
calculateAsymptotics SExpr = OCoeff 1
calculateAsymptotics SSkip = OCoeff 0
calculateAsymptotics (SIf t e) = OMax (calculateAsymptotics t) (calculateAsymptotics e)
calculateAsymptotics (SCounterFor _ var body) = OProd (OVar var) (calculateAsymptotics body)
calculateAsymptotics (SBlock exprs) = foldr (OSum . calculateAsymptotics) (OCoeff 0) exprs


sillyBubbleSort :: SillyAST
sillyBubbleSort = SBlock [
    SExpr,
    SCounterFor "i" "n" (
        SCounterFor "j" "n" (
            SIf SExpr SSkip
        )
    ),
    SExpr]
