module Par

where
    import           Data.Char

    data Token = TokenNum Int | TokenAlph String deriving Show

    tokenize :: String -> [Token]
    tokenize [] = []
    tokenize (s:rest) 
        | isDigit s = 
            let p = numberStr (s:rest)
            in
            TokenNum (read (fst p)) : tokenize (snd p)
        | otherwise = 
            let p = idStr (s:rest) in 
            TokenAlph (fst p) : tokenize (snd p)

    numberStr :: String -> (String, String)
    numberStr [] = error "unable to convert a empty string to a double"
    numberStr (s:rest) = numberStrZ [] s rest
        where
            numberStrZ acc c [] | isDigit c = (acc ++ [c], []) | otherwise = (acc, [c])
            numberStrZ acc c (r:rs) | isDigit c = numberStrZ (acc ++ [c]) r rs | otherwise = (acc, c:(r:rs))

    idStr :: String -> (String, String)
    idStr [] = error "unable to convert a empty string to a id"
    idStr (s:rest) = idStrZ [] s rest
        where
            idStrZ acc c []  | isAlpha c = (acc++[c], []) | otherwise = (acc, [c]) 
            idStrZ acc c (r:rs) | isAlpha c = idStrZ (acc ++ [c]) r rs | otherwise = (acc, c:(r:rs))

