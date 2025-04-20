module Math.Ring where 

-- Represents ring with the identity element for multiplication 
class Eq d => Ring d where
   radd :: d -> d -> d
   rmul :: d -> d -> d
   rneg :: d -> d
   rsub :: d -> d -> d
   rsub a b = a `radd` rneg b
   rzero :: d
   rone :: d

removeLeadingZeroes :: Ring d => [d] -> [d]
removeLeadingZeroes vals = let raw = reverse $ dropWhile (== rzero) $ reverse vals in
  if null raw then [rzero] else raw


-- Operations on polynomials ( d[x] ), where d is a Ring
addPoly :: Ring d => [d] -> [d] -> [d]
addPoly a b = removeLeadingZeroes $ uncurry radd <$> zipPadded a b
  where
    zipPadded [] ys = map (rzero,) ys
    zipPadded xs [] = map (, rzero) xs
    zipPadded (x:xs) (y:ys) = (x, y) : zipPadded xs ys

prodPoly :: Ring d => [d] -> [d] -> [d]
prodPoly [] _ = [rzero]
prodPoly (x : xs) ys = addPoly (rzero : prodPoly xs ys) (fmap (`rmul` x) ys)

rsumMany :: Ring d => [d] -> d
rsumMany [] = rzero
rsumMany arr = foldr1 radd arr

symbolicPower :: Ring d => Int -> d -> d
symbolicPower 0 _ = rone
symbolicPower n expr = foldr1 rmul $ replicate n expr
