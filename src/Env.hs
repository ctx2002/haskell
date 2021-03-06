module Env (

) where


import           Data.Char
import qualified Data.Text as T


data ExpVal = ExpInt Int | ExpBool Bool | 
    ExpEmpty | 
    ExpList [ExpVal] | 
    ExpProc Expression HENV deriving (Show)

data DenVal = DenInt Int | DenBool Bool deriving (Show)

--data VarExpr = VarExpr String deriving (Show)
--data VarExpr = Var String deriving (Show)
data Expression  = CNumber Int |
                   BinExpr Expression Expression Token | 
                   IsZero Expression |
                   IfExpr Expression Expression Expression |
                   VarExpr String |
                   LetExpr Expression  Expression Expression | 
                   Minus Int |
                   EmptyList | 
                   ProcExpr Expression Expression | 
                   CallExpr Expression Expression
                   deriving (Show)

data Program = Expression deriving (Show)
data HENV  = EnvEmpty | EnvAppend String ExpVal HENV deriving (Show)

runProgram :: String -> ExpVal
runProgram str = valueOf ( fst $ parseLet $ tokenizer str) EnvEmpty

envApply :: HENV -> String -> ExpVal
envApply EnvEmpty str = error (str ++ " can not be found.")
envApply (EnvAppend key value env) search  |  search == key = value
    | otherwise = envApply env search

extEnv :: String -> ExpVal -> HENV -> HENV
--extEnv key value EnvEmpty = EnvAppend key value EnvEmpty
--extEnv key value env = EnvAppend key value env
extEnv  = EnvAppend

isEmptyEnv :: HENV -> Bool
isEmptyEnv EnvEmpty = True
isEmptyEnv env      = False

numVal :: Int -> ExpVal
numVal  = ExpInt

boolVal :: Bool -> ExpVal
boolVal  = ExpBool

procVal :: Expression -> HENV -> ExpVal
procVal p@(ProcExpr ex1 ex2) env = ExpProc p env
procVal other env = error $ "Expected ProcExpr in procVal, instead of " ++ show other

expVal2Num :: ExpVal -> Int
expVal2Num struct = case struct of
                        ExpInt n -> n
                        _  -> error "Expected ExpInt , but ExpBool is been presented."

expVal2Proc :: ExpVal -> ExpVal
expVal2Proc struct = case struct of
    ExpProc exp env -> ExpProc exp env
    _  -> error "Expected ExpProc"

expVal2Bool :: ExpVal -> Bool
expVal2Bool struct = case struct of
                          ExpBool n -> n
                          _ -> error "Expected ExpBool, but ExpInt is been presented."

valueOf :: Expression -> HENV -> ExpVal
valueOf (CNumber n) env = numVal n
valueOf (Minus n) env   = numVal $ -(n) 
valueOf (VarExpr str) env = envApply env str
valueOf EmptyList env = ExpEmpty
valueOf (BinExpr ex1 ex2 op) env = case op of
    TAdd -> numVal ( expVal2Num( valueOf ex1 env)  + expVal2Num( valueOf ex2 env) )
    TMinus -> numVal ( expVal2Num( valueOf ex1 env)  - expVal2Num( valueOf ex2 env) )
    TMul ->   numVal ( expVal2Num( valueOf ex1 env)  * expVal2Num( valueOf ex2 env) )
    TCons -> let l = valueOf ex1 env 
                 r = valueOf ex2 env 
             in  
                case (l,r) of 
                    (ExpEmpty,ExpEmpty) -> ExpList []
                    (ExpEmpty,_) -> ExpList [r]
                    (_,ExpEmpty) -> ExpList [l]
                    (_,_) -> ExpList [l,r]

valueOf (IsZero exp1) env = if expVal2Num (valueOf exp1 env) == 0 then ExpBool True else ExpBool False
valueOf (IfExpr ex1 ex2 ex3) env  = if expVal2Bool (valueOf ex1 env) then valueOf ex2 env else valueOf ex3 env
valueOf (LetExpr ex0 ex1 ex2) env =
    case ex0 of
         VarExpr str -> let var1 = valueOf ex1 env
                            in valueOf ex2 (extEnv str var1 env)
         _ -> error "Expected VarExpr in LetExpr Expression."

valueOf p@(ProcExpr ex1 ex2) env = procVal p env
valueOf (CallExpr callId parameter) env = 
    let ex1 = valueOf callId env
        proc = expVal2Proc ex1
        arg = valueOf parameter env
        in
            applyProc proc arg

applyProc :: ExpVal -> ExpVal -> ExpVal
applyProc (ExpProc ex1 env) ex2 = case ex1 of
    ProcExpr (VarExpr str) body -> let env' = extEnv str ex2 env
        in
            valueOf body env'

data Token = TNumber Int | TIdent String
    | TAssign | TMinus | TAdd | TMul
    | TIn | TLet | TZero | TCons
    | TIf | TThen | TElse | TR | TL | TCommas
    | TNeg Int | TEmptyList | TProc
    | TEmpty deriving (Show , Eq)

lookahead :: [Token] -> Token
lookahead [] = TEmpty
lookahead (t:ts) = t

accept :: [Token] -> [Token]
--accept [] = error "Nothing to accept"
accept [] = []
accept (t:ts) = ts

tokenizer :: String -> [Token]
tokenizer [] = []
tokenizer (x:xs) | x == '=' = TAssign : tokenizer xs
                 | x == '-' = TMinus : tokenizer xs
                 | x == '+' = TAdd : tokenizer xs
                 | x == '*' = TMul : tokenizer xs
                 | x == ' ' = tokenizer xs
                 | x == '\n' = tokenizer xs
                 | x == '(' = TL : tokenizer xs
                 | x == ')' = TR : tokenizer xs
                 | x == ',' = TCommas : tokenizer xs
                 | otherwise = let t = getToken (x:xs)
                     in
                        strtoken (fst t) : tokenizer (snd t)

getToken :: String -> (String, String)
getToken []     = ([], [])
getToken (x:xs) | isDigit x = span isDigit (x:xs)
    | isAlpha x = span isZeroLetter (x:xs)
    | otherwise = (x:xs , [])

isZeroLetter :: Char -> Bool
isZeroLetter c = isAlpha c || c == '?' 

multiCharToken :: String -> String -> (String, String)
multiCharToken [] accu     = ([], accu)
multiCharToken (x:xs) accu | isDigit x = multiCharToken xs (accu ++ [x])
    | isAlpha x = multiCharToken xs (accu ++ [x])
    | otherwise = (xs, accu)



strtoken :: String -> Token
strtoken [] = TEmpty
strtoken (x:xs) | isDigit x = TNumber (read (x:xs))
    | isAlpha x = case x:xs of
        "in"   -> TIn
        "if"   -> TIf
        "then" -> TThen
        "else" -> TElse
        "let"  -> TLet
        "zero?" -> TZero
        "cons" -> TCons
        "emptylist" -> TEmptyList
        "proc" -> TProc
        _      -> TIdent (x:xs)
    | otherwise = error "wrong token"

parseBin :: [Token] -> (Expression, [Token])
parseBin (TMinus:TNumber n : toks) = ( Minus n, toks)
parseBin (TAdd:TNumber n: toks) = (CNumber n, toks)
parseBin (t:toks) = parseBinExpr toks t


parseBinExpr :: [Token] -> Token -> (Expression, [Token])
parseBinExpr [] op = error "expected BinExpr instead of empty."
parseBinExpr toks op =
     case lookahead toks of
         TL -> let
                  (lex, ltoks) = parseLet (accept toks)
                  (commas, toks') = if lookahead ltoks == TCommas then
                                       (TCommas ,accept ltoks)
                                    else error $ "Expected , in BinExpr, instead of " ++ show (lookahead ltoks)
                  (rex, rtoks) = parseLet toks'
              in
                  if lookahead rtoks == TR then
                     (BinExpr lex rex op, accept rtoks)
                  else error $ "expected ) in BinExpr, instead of " ++  show (lookahead rtoks)
         _ -> error $ "expected ( in BinExpr, instead of " ++  show (lookahead toks)


parseLet :: [Token] -> (Expression, [Token])
parseLet toks = case lookahead toks of
    TNumber n -> (CNumber n, accept toks)
    TMinus -> parseBin toks 
    TAdd -> parseBin toks
    TMul -> parseBin toks
    TZero -> parseLet (accept toks)
    TIf -> parseIf (accept toks)
    TLet -> parseL (accept toks)
    TIdent x -> (VarExpr x, accept toks)
    TCons -> parseBin toks
    TEmptyList -> (EmptyList, accept toks)
    TProc -> parseProc (accept toks)
    TL -> parseCall (accept toks)
    _ -> error $ "wrong token " ++ show (lookahead toks)

parseProc :: [Token] -> (Expression, [Token])
parseProc toks = case lookahead toks of 
    TL -> let (id, rest) = parseLet (accept toks)
          in 
            if lookahead rest == TR 
                then let (body , toks') = parseLet (accept rest)
                     in 
                        (ProcExpr id body, toks')
                else
                    error $ "Expected TR , instead of " ++ show (lookahead rest)
    _ -> error $ "Expected TL, instead of " ++ show (lookahead toks)

parseCall :: [Token] -> (Expression, [Token])
parseCall toks = 
    let 
        (call, rest) = parseLet toks
        (parameter, rest') = parseLet rest
    in
        if lookahead rest' == TR 
            then
                (CallExpr call parameter, accept rest' )
            else
                error $ "Expected TR , instead of " ++ show (lookahead rest')       
    

parseL :: [Token] -> (Expression, [Token])
parseL [] = error "expected LetExpr instead of empty."
parseL toks = 
    
    let 
        (ex1, toks')   = parseLet toks
        (ex2, toks'')  = if lookahead toks' == TAssign then parseLet (accept toks') 
            else error $ "In LetExpr expected =, instead of " ++ show (lookahead toks')
        (ex3, toks''') = if lookahead toks'' == TIn then parseLet (accept toks'') 
            else error $ "In LetExpr expected in, instead of " ++ show (lookahead toks'')
    in
        ( LetExpr ex1 ex2 ex3, toks''')   

parseIf :: [Token] -> (Expression, [Token])
parseIf [] = error "expected IfExr instead of empty."
parseIf toks = let 
        (ex1, ftoks)  = parseLet toks
        (ex2, stoks)  = if lookahead ftoks == TThen then parseLet (accept ftoks) 
            else error $ "In InExpr expected then , instead of " ++ show (lookahead ftoks)
        (ex3, ttoks)  = if lookahead stoks == TElse then parseLet (accept stoks) 
            else error $ "In InExpr expected else , instead of " ++ show (lookahead stoks)
    in
        ( IfExpr ex1 ex2 ex3, ttoks)         
