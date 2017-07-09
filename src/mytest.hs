module MYTEST

where
data ShishKebab =
    Skewer
    | Onion ShishKebab
    | Lamb ShishKebab
    | Tomato ShishKebab

onlyOnion :: ShishKebab -> Bool
onlyOnion Skewer     = True
onlyOnion (Onion x)  = onlyOnion x
onlyOnion (Lamb x)   = False
onlyOnion (Tomato x) = False

data CreditCardNumber a =
    UserCreditCardNumber a
    | VISANumber (CreditCardNumber a)
    | AMAXNumber (CreditCardNumber a)
    | MASTENumber (CreditCardNumber a) deriving Show

isVISANumber :: CreditCardNumber a -> Bool
isVISANumber (VISANumber x)           = True
isVISANumber (UserCreditCardNumber x) = False
isVISANumber (AMAXNumber x)           = False
isVISANumber (MASTENumber x)          = False

data CreditCardSecureNumber a =
    UserCreditCardSecureNumber a
    | VISASecureCode (CreditCardSecureNumber a)
    | AMAXSecureCode (CreditCardSecureNumber a)
    | MASTERSecureCode (CreditCardSecureNumber a) deriving Show

data CreditCard = CreditCard {
    number       :: CreditCardNumber String,
    secureNumber :: CreditCardSecureNumber String
} deriving Show

myfib :: [Integer] -> Integer -> Integer
myfib accumulator n | n == 0 = head  accumulator
           | n == 1 = last accumulator
           | otherwise = myfib [last accumulator , sum accumulator] (n-1)

myfib1 :: Integer -> Integer -> Integer -> Integer
myfib1 firstNumer secondNumber index | index == 0 = firstNumer
    | index == 1 = secondNumber
    | otherwise = myfib1 secondNumber (firstNumer+secondNumber) (index-1)

exFib :: Integer -> Integer
exFib n | n < 0 = error "Negative number."
        | otherwise = myfib1 0 1 n

expense :: Float -> Float
expense weight | 0 < weight && weight <= 60 = weight * 0.1
    | 60 < weight && weight <=120 = weight * 0.2
    | 120 < weight && weight <= 160 = weight * 0.3
    | 160 < weight && weight <= 220 = weight * 0.4
    | otherwise = weight * 0.5

feq :: Float -> Float -> Bool
-- compare 2 floats, we can understand as x approching y
-- which means sequence x use y as limit
feq x y = abs(x - y) < 1e-6

lione :: Float -> Float
lione x = sin x + cos x

cn :: Float -> Float
cn n = n / (3**n)

sn :: Float -> Float
sn x = 1 / ((3 * x) + 1)

bn :: Float -> Float
bn n = ((3*n)+1) / ((4*n)-1)
dn :: Float -> Float
dn n = sin( (n*3.14159) / 4.0 )

en :: Float -> Float
en n = cos ((n*3.14159)/3)


