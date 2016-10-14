module TAMO (
    (==>), (<+>), (<=>)
)where

infix 1 ==>
(==>) :: Bool -> Bool -> Bool
x ==> y = not x || y

infix 1 <=>
(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

infix 2 <+>
(<+>) :: Bool -> Bool -> Bool
x <+> y = x /= y

valid1 :: (Bool -> Bool) -> Bool
valid1 bf = bf True && bf False

excludedMiddle :: Bool -> Bool
excludedMiddle p = p || not p

valid2 :: (Bool -> Bool -> Bool) -> Bool
valid2 bf = bf True True
            && bf True False
            && bf False True
            && bf False False

contradiction1 :: (Bool -> Bool) -> Bool
contradiction1 bf = not (bf True || bf False)
-- c1 = p && (not p)

contradiction2 :: (Bool -> Bool -> Bool) -> Bool
contradiction2 bf = not (or [ bf q p | q<-[True,False],p<-[True, False]])

contradiction3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
contradiction3 bf = not (or [bf p q r | p<-[True, False], q<-[True,False],r<-[True,False]])


-- f1 p q = (p ==> (not q)) <=> (q ==> (not p))
-- deM1 p q = (not (p && q))  <=> (not p) || (not q)

valid3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
valid3 bf =
    and [bf p q r | p <-[True, False],q <-[True, False],r <-[True, False]]
-- f2 p q r = ((p && q) || (not r)) <=> ((q || (not r)) && (q || (not q)))

valid4 bf = and [bf p q r s | p <- [True, False],q <-[True, False], r <-[True, False], s <- [True, False]]

logEquiv1 :: (Bool -> Bool) -> (Bool -> Bool) ->Bool
logEquiv1 bf1 bf2 = and [bf1 p <=> bf2 p | p<-[True, False]]

logEquiv2 :: (Bool->Bool->Bool) -> (Bool->Bool->Bool) -> Bool
logEquiv2 bf1 bf2 = and [bf1 p q <=> bf2 p q | p<-[True,False], q<-[True, False]]


