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


