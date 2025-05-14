module Logarithmical.OExpr where

import Control.Monad.State qualified as State
import Data.Map qualified as Map
import Data.Ratio (denominator, numerator)
import Data.Set qualified as Set
import Math.Faulhaber (faulhabersCoefs)
import Math.Multinomial (Multinomial (MCoeff, MVar), OCoeff, OVar)
import Math.Ring

-- TODO: make OExpr isorecursive type like `Fix OExprShape` or a tagless final interpreter to
-- remove boilerplate

-- Represent how loop variant is variated on each loop iteration
data OCounterVariant
  = OIncrenmentCounter
  | OMultiplyCounter
  deriving (Eq, Show, Read, Ord)

data OExpr
  = OVar OVar -- variable (free or binded via OCounter)
  | OCoeff OCoeff -- constant
  | OSum OExpr OExpr -- Just sum
  | OProd OExpr OExpr -- Just multiplication
  | OMax OExpr OExpr -- Just majorisation
  | OCounter OVar OExpr OExpr OCounterVariant OExpr -- Sum for ... from ... to ... of ...
  | OCall OVar OExpr -- Call to functional synbol, allows recursion
  | OLog OExpr -- Call to logarithm Function
  deriving (Eq, Show, Read, Ord)

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

prettyExpr :: OExpr -> String
prettyExpr (OCoeff n) =
  if denominator n /= 1
    then
      "(" ++ show (numerator n) ++ " / " ++ show (denominator n) ++ ")"
    else
      show $ numerator n
prettyExpr (OVar var) = var
prettyExpr (OSum lhs rhs) = "(" ++ prettyExpr lhs ++ " + " ++ prettyExpr rhs ++ ")"
prettyExpr (OProd lhs rhs) = "(" ++ prettyExpr lhs ++ " * " ++ prettyExpr rhs ++ ")"
prettyExpr (OMax lhs rhs) = "max {" ++ prettyExpr lhs ++ ", " ++ prettyExpr rhs ++ "}"
prettyExpr (OCounter c f t _ e) = "(Sum for " ++ c ++ " = " ++ prettyExpr f ++ " to " ++ prettyExpr t ++ " of " ++ prettyExpr e ++ ")"
prettyExpr (OCall functionName e) = functionName ++ "(" ++ prettyExpr e ++ ")"
prettyExpr (OLog e) = "log(" ++ prettyExpr e ++ ")"

multinomialToExpr :: Multinomial -> OExpr
multinomialToExpr (MCoeff coef) = OCoeff coef
multinomialToExpr (MVar var coefs) = substPoly (fmap multinomialToExpr coefs) (OVar var)

substExpr :: (OExpr -> Bool) -> (OExpr -> OExpr) -> OExpr -> OExpr
substExpr predicate s e
  | predicate e = s e
  | otherwise = case e of
      OSum l r -> OSum (substExpr predicate s l) (substExpr predicate s r)
      OProd l r -> OProd (substExpr predicate s l) (substExpr predicate s r)
      OMax l r -> OMax (substExpr predicate s l) (substExpr predicate s r)
      OCounter v from to op body -> OCounter v (substExpr predicate s from) (substExpr predicate s to) op (substExpr predicate s body)
      OCall functionName argument -> OCall functionName (substExpr predicate s argument)
      OLog e -> OLog (substExpr predicate s e)
      v -> v

constFolding :: OExpr -> OExpr
constFolding v@(OVar _) = v
constFolding c@(OCoeff _) = c
constFolding (OSum lhs rhs) = case (constFolding lhs, constFolding rhs) of
  (OCoeff l, OCoeff r) -> OCoeff $ l + r
  (OCoeff 0, r) -> r
  (l, OCoeff 0) -> l
  (l, r) -> OSum l r
constFolding (OProd lhs rhs) = case (constFolding lhs, constFolding rhs) of
  (OCoeff l, OCoeff r) -> OCoeff $ l * r
  (OCoeff 1, r) -> r
  (l, OCoeff 1) -> l
  (l, r) -> OProd l r
constFolding (OMax lhs rhs) = case (constFolding lhs, constFolding rhs) of
  (OCoeff l, OCoeff r) -> OCoeff $ max l r
  (l, r) -> OMax l r
constFolding (OCounter c f t op e) = OCounter c (constFolding f) (constFolding t) op (constFolding e)
constFolding (OCall name e) = OCall name $ constFolding e
constFolding (OLog e) = OLog $ constFolding e -- logarithm can be not representable as rational (see OCoeff definition TODOs)

collectCalls :: OVar -> OExpr -> [OExpr]
collectCalls _ (OVar _) = []
collectCalls _ (OCoeff _) = []
collectCalls f (OSum lhs rhs) = collectCalls f lhs ++ collectCalls f rhs
collectCalls f (OProd lhs rhs) = collectCalls f lhs ++ collectCalls f rhs
collectCalls f (OMax lhs rhs) = collectCalls f lhs ++ collectCalls f rhs
collectCalls f (OCounter _ _ _ _ e) = collectCalls f e -- counter final / initial value should not contain call
collectCalls f (OCall f' e)
  | f == f' = [e]
  | otherwise = []
collectCalls f (OLog e) = []

collectCalled :: OExpr -> Set.Set OVar
collectCalled (OVar _) = Set.empty
collectCalled (OCoeff _) = Set.empty
collectCalled (OSum lhs rhs) = collectCalled lhs `Set.union` collectCalled rhs
collectCalled (OProd lhs rhs) = collectCalled lhs `Set.union` collectCalled rhs
collectCalled (OMax lhs rhs) = collectCalled lhs `Set.union` collectCalled rhs
collectCalled (OCounter c f t op e) = collectCalled e -- counter final / initial value should not contain call
collectCalled (OCall f e) = Set.singleton f
collectCalled (OLog e) = Set.empty

freeVars :: OExpr -> Set.Set OVar
freeVars (OCoeff _) = Set.empty
freeVars (OVar name) = Set.singleton name
freeVars (OProd lhs rhs) = freeVars lhs `Set.union` freeVars rhs
freeVars (OSum lhs rhs) = freeVars lhs `Set.union` freeVars rhs
freeVars (OMax lhs rhs) = freeVars lhs `Set.union` freeVars rhs
freeVars (OCounter counter from to op expr) = counter `Set.delete` (freeVars expr `Set.union` freeVars from `Set.union` freeVars to)
freeVars (OCall _ expr) = freeVars expr
freeVars (OLog expr) = freeVars expr

-- Multinomial expressions utils: rewrites the expression as multinmials

isMultinomial :: OExpr -> Bool
isMultinomial (OVar _) = True
isMultinomial (OCoeff _) = True
isMultinomial (OSum lhs rhs) = isMultinomial lhs && isMultinomial rhs
isMultinomial (OProd lhs rhs) = isMultinomial lhs && isMultinomial rhs
isMultinomial (OMax lhs rhs) = False
isMultinomial (OCounter c from to op body) = case op of
  OIncrenmentCounter -> isMultinomial to && isMultinomial from && isMultinomial body
  _ -> False
isMultinomial (OCall _ _) = False
isMultinomial (OLog _) = False

-- receives a name of variable `n`, represents given expression
-- as `A * n^0 + B * n^1 + C * n ^ 2 + ...`, where A, B, C a expressions, which depends on
-- free variables of given expression only
withdrawVar :: OVar -> OExpr -> [OExpr]
withdrawVar _ v@(OCoeff _) = [v]
withdrawVar var v@(OVar var') = if var' == var then [OCoeff 0, OCoeff 1] else [v]
withdrawVar var (OProd lhs rhs) = prodPoly (withdrawVar var lhs) (withdrawVar var rhs)
withdrawVar var (OSum lhs rhs) = addPoly (withdrawVar var lhs) (withdrawVar var rhs)
withdrawVar var v@(OCall _ _) = [v]
withdrawVar var (OCounter counter from to op expr) =
  case op of
    OIncrenmentCounter ->
      let normalizedExpr = withdrawVar counter expr
       in let n = length normalizedExpr - 1
           in withdrawVar var $
                rsumMany
                  [ bJ `rmul` (substPoly pJ to `rsub` substPoly pJ from)
                    | j <- [0 .. n],
                      let pJ = fmap OCoeff $ faulhabersCoefs !! j,
                      let bJ = normalizedExpr !! j
                  ]
    _ -> undefined
withdrawVar _ _ = undefined

exprToMultinomial :: OVar -> OExpr -> Multinomial
exprToMultinomial name expr = exprToMultinomialImpl exprVars expr'
  where
    expr' = constFolding expr
    exprVars :: [OVar]
    exprVars = name : Set.elems (Set.delete name (freeVars expr'))
    exprToMultinomialImpl :: [OVar] -> OExpr -> Multinomial
    exprToMultinomialImpl [] (OCoeff v) = MCoeff v
    exprToMultinomialImpl [] _ = undefined -- TODO: throw/catch
    exprToMultinomialImpl (var : vars) expr = MVar var $ fmap (exprToMultinomialImpl vars . constFolding) (withdrawVar var expr)

type Bimap a b = (Map.Map a b, Map.Map b [a])

bimapInsert :: (Ord a, Ord b) => a -> b -> Bimap a b -> Bimap a b
bimapInsert a b (l, r) = (Map.insert a b l, Map.insert b (a : Map.findWithDefault [] b r) r)

bimapLookupL :: (Ord a, Ord b) => a -> Bimap a b -> b
bimapLookupL a (l, _) = (Map.!) l a

bimapLookupR :: (Ord a, Ord b) => b -> Bimap a b -> [a]
bimapLookupR b (_, r) = (Map.!) r b

bimapUnion :: (Ord a, Ord b) => Bimap a b -> Bimap a b -> Bimap a b
bimapUnion (l, r) (l', r') = (Map.union l l', Map.unionWith (++) r r')

bimapEmpty :: Bimap a b
bimapEmpty = (Map.empty, Map.empty)

bimapSingleton :: a -> b -> Bimap a b
bimapSingleton a b = (Map.singleton a b, Map.singleton b [a])

-- TODO: Explicify Monad.Writer
extractCalls :: OExpr -> (OExpr, Bimap OVar OExpr)
extractCalls expr = State.evalState (extractCallsImpl expr) 0
  where
    extractCallsImpl :: OExpr -> State.State Int (OExpr, Bimap OVar OExpr)
    extractCallsImpl v@(OVar _) = return (v, bimapEmpty)
    extractCallsImpl v@(OCoeff _) = return (v, bimapEmpty)
    extractCallsImpl v@(OLog e) = return (v, bimapEmpty)
    extractCallsImpl (OSum lhs rhs) = do
      (lhs', lmap) <- extractCallsImpl lhs
      (rhs', rmap) <- extractCallsImpl rhs
      return (OSum lhs' rhs', bimapUnion lmap rmap)
    extractCallsImpl (OProd lhs rhs) = do
      (lhs', lmap) <- extractCallsImpl lhs
      (rhs', rmap) <- extractCallsImpl rhs
      return (OProd lhs' rhs', bimapUnion lmap rmap)
    extractCallsImpl (OMax lhs rhs) = do
      (lhs', lmap) <- extractCallsImpl lhs
      (rhs', rmap) <- extractCallsImpl rhs
      return (OMax lhs' rhs', bimapUnion lmap rmap)
    extractCallsImpl (OCounter c from to op body) = do
      (body', bmap) <- extractCallsImpl body
      return (OCounter c from to op body', bmap)
    extractCallsImpl v@(OCall name e) = do
      fresh <- State.get
      State.put $ fresh + 1
      let varName = "%%CALL_" ++ show fresh
      return (OVar varName, bimapSingleton varName v)

-- Represents a bunch of functions: function_name |-> (argument_name, function_body)
type FunctionContext = Map.Map OVar (OVar, OExpr)

lookupCtx :: FunctionContext -> OVar -> (OVar, OExpr)
lookupCtx = (Map.!)

applyOExprCall :: FunctionContext -> OVar -> OExpr -> OExpr
applyOExprCall ctx functionName argument =
  let (argumentName, functionBody) = lookupCtx ctx functionName
   in substExpr (== OVar argumentName) (const argument) functionBody

inlineFunctions :: FunctionContext -> OVar -> (OVar, OExpr)
inlineFunctions ctx functionName =
  let (argumentName, functionBody) = lookupCtx ctx functionName
   in (argumentName, inlineStep (Set.singleton functionName) functionBody)
  where
    isCallTo :: OVar -> OExpr -> Bool
    isCallTo v (OCall v' _) = v == v'
    isCallTo _ _ = False

    extractArg :: OExpr -> OExpr
    extractArg (OCall _ e) = e
    extractArg _ = undefined

    inlineStep :: Set.Set OVar -> OExpr -> OExpr
    inlineStep barrier body =
      let called = collectCalled body
       in let callsToInline = called `Set.difference` barrier
           in let inlineFunction name = substExpr (isCallTo name) (inlineStep (name `Set.insert` barrier) . applyOExprCall ctx name . extractArg)
               in foldr inlineFunction body callsToInline

inlineInContext :: OVar -> FunctionContext -> FunctionContext
inlineInContext name ctx = Map.insert name (inlineFunctions ctx name) ctx

-- Should be applied to already inlined functions
isNonCalling :: FunctionContext -> OVar -> Bool
isNonCalling ctx name =
  Set.null $ collectCalled $ snd $ lookupCtx ctx name

-- Should be applied to already inlined functions
isSimpleRecursive :: FunctionContext -> OVar -> Bool
isSimpleRecursive ctx name =
  Set.singleton name == collectCalled (snd $ lookupCtx ctx name)

-- Should be applied to already inlined functions
-- TODO: may be we need to be more accurate
isMutualrecursive :: FunctionContext -> OVar -> Bool
isMutualrecursive ctx name = not (isNonCalling ctx name) && not (isSimpleRecursive ctx name)
