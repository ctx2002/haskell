module Algebra () where

    {- located vector-}
    data LocatedVector = LocatedV (Double, Double) (Double, Double)

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

    dotProduct :: [(Double, Double)] -> Double
    dotProduct = foldr ((+).uncurry (*)) 0
    {-
     for math dot product like (1,2,3) x (1,2,3) = 14
     but for this procudure, we use this form to represent 2 vectors dot production
     [(x1,y1), (x2,y2), (x3,y3)], so to use this function, so should do zip first.

       says, we want to add [1,2,3] [1,2,3], first zip [1,2,3] [1,2,3], get result
       [(1,1),(2,2), (3,3)], then feed this result to dotProduct
     -}

    betterDotProduct :: [Double] -> [Double] -> Double
    betterDotProduct  x y = dotProduct  (zip x y)

    dotProductPower :: [Double] -> Double
    dotProductPower x = betterDotProduct x x

    isPerpendicular :: [Double] -> [Double] -> Bool
    isPerpendicular x y = betterDotProduct x y == 0

    norm :: [Double] -> Double
    norm x  = sqrt $ betterDotProduct x x



    {- some function related to vector -- serg lang -- intro to algebra-}
    locatedVectorMinus :: [Double] -> [Double] -> [Double]
    locatedVectorMinus start end = zipWith (-) end start

    {- if 2 located vector is equivalent see serg lang - intro to algebra page 9 -}
    locatedVectorIsEquivalent :: ([Double], [Double]) -> ([Double], [Double]) -> Bool
    locatedVectorIsEquivalent first second =  uncurry locatedVectorMinus first  == uncurry locatedVectorMinus second
    {- scalar product-}
    vectorScalarProduct :: [Double] -> [Double] -> Double
    vectorScalarProduct f s = sum $ zipWith (*) f s

    vectorAddition :: [Double] -> [Double] -> [Double]
    vectorAddition = zipWith (+)

    vectorMinus :: [Double] -> [Double] -> [Double]
    vectorMinus = zipWith (-)

    vectorMultilcateNumber :: Double -> [Double] -> [Double]
    vectorMultilcateNumber n = map (*n)

    verctorIsPerpendicular :: [Double] -> [Double] -> Bool
    verctorIsPerpendicular f s = 0 == vectorScalarProduct f s

    vectorCosin :: [Double] -> [Double] -> Double
    vectorCosin a b = (/) (vectorScalarProduct a b) ((*) (norm a) (norm b))

    vectorProjection :: [Double] -> [Double] -> Double
    vectorProjection a b = (/) (vectorScalarProduct a b) (vectorScalarProduct b b)

    matrixAdd :: [[Double]] -> [[Double]] -> [[Double]]
    matrixAdd [] [] = []
    matrixAdd a b = vectorAddition (head a) (head b) : matrixAdd (tail a) (tail b)

    numberTimeMatrix :: Double -> [[Double]] -> [[Double]]
    numberTimeMatrix n [] = []
    numberTimeMatrix n m  = map (vectorMultilcateNumber n)  m
    {-
        matrixAdd [[1,2,3], [-1,0,2]] $ matrixAdditiveInverse [[1,2,3], [-1,0,2]]
    -}
    matrixAdditiveInverse :: [[Double]] -> [[Double]]
    matrixAdditiveInverse = numberTimeMatrix (-1)

    vector2Matrix :: [Double] -> [[Double]]
    vector2Matrix [] = []
    vector2Matrix v  = map (: []) v
    {-
        matrixT [[2,1,0],[1,3,5]]
    -}
    matrixT :: [[Double]] -> [[Double]]
    matrixT []     = []
    matrixT ([]:_) = []
    matrixT x      = map head x : matrixT (map tail x)

    matrixMultiplication :: [[Double]] -> [[Double]] -> [[Double]]
    matrixMultiplication a b  = matrixMultiplicationS a (matrixT b)

    matrixMultiplicationS :: [[Double]] -> [[Double]] -> [[Double]]
    matrixMultiplicationS [] [] = []
    matrixMultiplicationS [] b  = []
    matrixMultiplicationS a b = matrixMultiplicationT (head a) b :  matrixMultiplicationS (tail a) b

    matrixMultiplicationT :: [Double] -> [[Double]] -> [Double]
    matrixMultiplicationT a [] = []
    matrixMultiplicationT a b = vectorScalarProduct a (head b) : matrixMultiplicationT a (tail b)
