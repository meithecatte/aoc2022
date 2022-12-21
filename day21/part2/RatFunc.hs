import Data.Ratio

newtype Polynomial = Poly [Integer]
    deriving Show

add [] p = p
add p [] = p
add (x:xs) (y:ys) = (x+y):add xs ys

mulx (Poly xs) = Poly $ 0:xs
mulc c (Poly xs) = Poly $ map (c *) xs

instance Num Polynomial where
    fromInteger 0 = Poly []
    fromInteger n = Poly [n]

    Poly xs + Poly ys = Poly $ add xs ys

    Poly [] * p = 0
    Poly (x:xs) * p = mulc x p + mulx (Poly xs * p)

    negate (Poly xs) = Poly $ map negate xs

data RatFunc = Rat !Polynomial !Polynomial
    deriving Show

instance Num RatFunc where
    fromInteger n = Rat (fromInteger n) 1

    (Rat a b) + (Rat c d) = Rat (a*d + b*c) (b * d)
    (Rat a b) * (Rat c d) = Rat (a * c) (b * d)

    negate (Rat a b) = Rat (negate a) b

instance Fractional RatFunc where
    recip (Rat a b) = Rat b a
    fromRational r = Rat (fromInteger $ numerator r) (fromInteger $ denominator r)

humn = Rat (Poly [0, 1]) 1

-- ax + b = 0
-- x = -b/a
polyroot (Poly [b, a]) = (- b) % a
solve (Rat a b, Rat c d) = polyroot $ a * d - b * c
