{-# LANGUAGE RankNTypes #-}
module LearnLamChap2
()

where

    myIden :: a -> a
    myIden = \x -> x

    mySelf ::  (forall a. a -> a) -> b -> b
    mySelf = \s -> (s s)

    funcArg :: a -> a -> b -> b
    funcArg = \func -> \arg -> (func arg)
