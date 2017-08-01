module Env (

) where

data ExpVal = ExpInt Int | ExpBool Bool 
data DenVal = DenInt Int | DenBool Bool

--data VarExpr = VarExpr String deriving (Show)
--data VarExpr = Var String deriving (Show)
data Expression  = CNumber Int |
                   DiffExpr Expression Expression |
                   IsZero Expression |
                   IfExpr Expression Expression Expression |
                   VarExpr String |
                   LetExpr Expression  Expression Expression
                   deriving (Show)

data Program = Expression deriving (Show)

data HENV  = EnvEmpty | EnvAppend String ExpVal HENV 

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
isEmptyEnv env = False

numVal :: Int -> ExpVal
numVal  = ExpInt

boolVal :: Bool -> ExpVal
boolVal  = ExpBool

expVal2Num :: ExpVal -> Int
expVal2Num struct = case struct of
                        ExpInt n -> n   
                        _  -> error "Expected ExpInt , but ExpBool is been presented."

expVal2Bool :: ExpVal -> Bool
expVal2Bool struct = case struct of
                          ExpBool n -> n
                          _ -> error "Expected ExpBool, but ExpInt is been presented."

valueOf :: Expression -> HENV -> ExpVal
valueOf (CNumber n) env = numVal n
valueOf (VarExpr str) env = envApply env str
valueOf (DiffExpr ex1 ex2)  env = numVal ( expVal2Num( valueOf ex1 env)  - expVal2Num( valueOf ex2 env) )
valueOf (IsZero exp1) env = if expVal2Num (valueOf exp1 env) == 0 then ExpBool True else ExpBool False
valueOf (IfExpr ex1 ex2 ex3) env  = if expVal2Bool (valueOf ex1 env) then valueOf ex2 env else valueOf ex3 env 
valueOf (LetExpr ex0 ex1 ex2) env = 
    case ex0 of
         VarExpr str -> let var1 = valueOf ex1 env
                            in valueOf ex2 (extEnv str var1 env)
         _ -> error "Expected VarExpr in LetExpr Expression."