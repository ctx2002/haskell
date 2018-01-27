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
