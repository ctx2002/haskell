module TAMO (
    (==>), (<+>), (<=>)
)where
import           Data.Char

infix 1 ==>
(==>) :: Bool -> Bool -> Bool
x ==> y = not x || y

infix 1 <=>
(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

infix 2 <+>
(<+>) :: Bool -> Bool -> Bool
x <+> y = x /= y

allT p = True
allF p = False

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

logEquiv3 :: (Bool -> Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool -> Bool) -> Bool
logEquiv3 bf1 bf2 = and [bf1 p q r <=> bf2 p q r | p<-[True,False],q<-[True,False],r<-[True,False]]

test0 = logEquiv1 (\ p -> not  (allF p)) (\ p -> allT p)
test01 = logEquiv1 (\ p -> not (allT p)) (\ p -> allF p)
test02 = logEquiv1 (\ p -> p ==> allF p) (\ p -> not p)
-- test1 = logEquiv1 id (\ p -> not (not p))
test1 = logEquiv1 id (not . not)
test2a = logEquiv1 id (\p -> p && p)
test2b = logEquiv1 id (\p -> p || p)
test3a = logEquiv2 (\ p q -> p ==> q) (\ p q -> not p || q)
test3b = logEquiv2 (\ p q -> not (p ==> q)) (\ p q -> p && not q)
test4a = logEquiv2 (\ p q -> not p ==> not q) (\ p q -> q ==> p)
test4b = logEquiv2 (\ p q -> p ==> not q) (\ p q -> q ==> not p)
test4c = logEquiv2 (\ p q -> not p ==> q) (\ p q -> not q ==> p)
test5a = logEquiv2 (\ p q -> p<=>q) (\ p q -> (p==>q) && (q==>p))
test5b = logEquiv2 (\ p q -> p<=>q) (\ p q -> (p && q) || (not q && not p))

-- You can eta-reduce blowupCreditCardNumber
blowupCreditCardNumber :: String -> [(Int,Char)]
blowupCreditCardNumber  = zip [1..]
-- blowupCreditCardNumber creditCardNumber = zip [1..] creditCardNumber

creditCardDouble :: [(Int,Char)] -> [Int]
--creditCardDouble [] = []
--creditCardDouble ((index,digit):rest)
--    | even index = (numberDoubleToList ((*2) $ digitToInt digit)) ++ creditCardDouble rest
--    | otherwise = digitToInt digit : creditCardDouble rest

creditCardDouble = concatMap foo where
  foo (index, digit) | even index = numberDoubleToList $ (*2) $ digitToInt digit
                     | otherwise = [digitToInt digit]


numberDoubleToList:: Int -> [Int]
numberDoubleToList = map digitToInt . show
--numberDoubleToList number
--    | number > 9 = map digitToInt (show number)
--    | otherwise  = [number]

creditCardReminder :: [Int] -> Int
creditCardReminder xs = sum xs `mod` 10

isCreditCardNumber :: String -> Bool
isCreditCardNumber = (==0) . creditCardReminder . creditCardDouble . blowupCreditCardNumber . reverse

-- isCreditCardNumber = (==0) . (`mod` 10) . sum . concatMap (digitToInt . show) . zipWith (*) (cycle [1, 2]) . map digitToInt . reverse


