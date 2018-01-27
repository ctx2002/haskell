module Algebra () where
    poly :: [(Double, Double)] -> Double
    -- | uncurry converts a curried function to a function on pairs

    poly = foldr ((+).uncurry (*)) 0

    {-
        poly (x:xs) = (fst x) * (snd x) + poly xs , first version
        second verion, use lambda
        poly xs = foldr (\x -> (+) (fst x * snd x) ) 0 xs ,

        since x is pair and * is a binary function, we can replace it with uncurry
        poly xs = foldr (\x -> (+) (uncurry (*) x)) 0 xs

        haskell does not like lambda, use function compose instead
        poly xs = foldr ((+).uncurry (*)) 0 xs

        eta reduce

        poly = foldr ((+).uncurry (*)) 0

    -}
    -- polynomia [(2,0), (3,1),(0,2), (4,3)] 7
    -- is 2x^0 + 3x^1 + 0x^2 + 4x^3, x is 7
    polynomia :: [(Double , Integer)] -> Double -> Double
    polynomia coefficents variable = poly $ map (\x -> (fst x , (^) variable (snd x)  )) coefficents
    -- polynomia coefficents variable = map (\x -> (fst x , (**) variable (snd x))) coefficents
