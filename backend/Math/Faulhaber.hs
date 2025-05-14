module Math.Faulhaber where

import Data.Ratio (denominator, numerator, (%))

-- Since coefficients of Faulhaber's formula can be calculated only
-- with some complex operations, such as numerical integration,
-- I decided to hardcode it

faulhabersCoefs :: [[Rational]]
faulhabersCoefs =
  [ [0, 1],
    [0, 1 % 2, 1 % 2],
    [0, 1 % 6, 1 % 2, 1 % 3],
    [0, 0, 1 % 4, 1 % 2, 1 % 4],
    [0, (-1) % 30, 0, 1 % 3, 1 % 2, 1 % 5],
    [0, 0, (-5) % 60, 0, 5 % 12, 1 % 2, 1 % 6],
    [0, 1 % 42, 0, (-5) % 30, 0, 1 % 2, 1 % 2, 1 % 7]
  ]
