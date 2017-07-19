module Par

where
    import           Data.Char

    data Token = TokenSpace | TokenNum Double  | TokenAlph String deriving Show

    tokenize :: String -> [Token]
    tokenize [] = []
    tokenize (s:rest) 
        | isDigit s = 
            let p = span isDigit (s:rest)
            in
            TokenNum (read (fst p)) : tokenize (snd p)
        | isAlpha s = 
            let p = span isAlpha(s:rest) in 
            TokenAlph (fst p) : tokenize (snd p)
        | isSpace s = 
            let p = span isSpace(s:rest) in 
            TokenSpace : tokenize (snd p)
        | otherwise = []

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

