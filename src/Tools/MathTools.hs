module Tools.MathTools where


-- Define approxEqual - note the apostrophe - with a tolerance input
approxEqual' :: (Fractional a, Ord a, Num a) => a -> a -> a -> Bool
approxEqual' x y tol = (abs (x - y)) < tol

-- Define wrapper with default value:
approxEqual :: (Fractional a, Ord a, Num a) => a -> a -> Bool
approxEqual x y = approxEqual' x y 1E-9
 
