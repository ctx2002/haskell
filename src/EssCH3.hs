module EssCH3 where
import Data.Maybe
import Data.Char
import Debug.Trace
{--
 - An environment is a function whose domain is a finite set of variables and
 - whose range is the denoted values
 -
 - --}
type  Environment  = [(String,DenVal)] 
apply_environment ::  Environment -> String -> DenVal
apply_environment [] key    = DenUndef
apply_environment (x:xs) key = if (fst x) == key then (snd x) else apply_environment xs key

extend_environment :: String -> DenVal -> Environment -> Environment
extend_environment key value old_environment = (key, value): old_environment

has_binding :: Environment -> String -> Bool
has_binding [] key = False
has_binding (x:xs) key = (fst x) == key ||  has_binding xs key

type IfCondition  = Expr 
type IfThen  = Expr 
type IfElse  = Expr 
type LetId    = Expr 
type LetExpr  = Expr 
type LetIn   = Expr 

data Expr  =  Number Integer | ExprEmpty
               | DiffExpr Expr  Expr  | AddExpr Expr  Expr 
               | ZeroExpr Expr  | NegExpr Expr 
               | IfExpr IfCondition  IfThen  IfElse -- IfCondition has to be a ZeroExpr
               | IdExpr String
               -- let f = proc (x) -(x,11)
               -- in (f (f 77))
               -- In (proc-exp var body), the variable var is the bound variable
               -- or formal
               -- parameter. In a procedure call (call-exp exp1 exp2), the
               -- expression exp1
               -- is the operator and exp2 is the operand or actual parameter
               | ProcExpr String Expr  | CallExpr Expr Expr
               | LetExpr LetId LetExpr  LetIn  deriving (Show)-- let id = expr in expr
                
{--
 - an important part of the specification of programming language is the set of
 - values that language manipulates, each language has at least 2 such sets:
 - the express values, and denoted values.
 -
 - The expressed values are the possible values of expressions, and the denoted
 - values are the values bound to variables.
 - 
 - read ExpVal as expression has an value
 - read ExpInt as expression int has an Integer value
 -
 - look, every data value need undefined status.
 - --}
data ExpVal = ExpProc String Expr  Environment | ExpInt Integer | ExpBool Bool | ExpUndef deriving (Show)
data DenVal = DenProc String Expr  Environment | DenInt Integer | DenBool Bool | DenUndef deriving (Show)

{--
 - to use above definition, we need an interface for the data type of expressed
 - values, our interface will have following entries.
 -
 - every expression must evaluate to a certain value. 
 - we need to define that certain value,which is why we have above definitions
 -
 - define operations on expression's value
 - --}

int2Val :: Integer -> ExpVal
int2Val = ExpInt

bool2Val :: Bool -> ExpVal
bool2Val = ExpBool

expVal2Int :: ExpVal -> Integer
expVal2Int (ExpInt n)  = n

expVal2Bool :: ExpVal -> Bool
expVal2Bool (ExpBool n) = n
expVal2Bool (ExpInt n)  = n /= 0
expVal2Bool ExpUndef    = False

denVal2ExpVal :: DenVal -> ExpVal
denVal2ExpVal (DenInt n) =  (ExpInt n)
denVal2ExpVal (DenBool n) = (ExpBool n)
denVal2ExpVal (DenProc var ex env) = (ExpProc var ex env)
denVal2Expval _         = ExpUndef

expVal2DenVal :: ExpVal -> DenVal
expVal2DenVal (ExpInt n) = (DenInt n)
expVal2DenVal (ExpBool n) = (DenBool n)
expVal2DenVal (ExpProc var ex env) = DenProc var ex env
expVal2DenVal _           = DenUndef

{--
constExpr :: Integer -> Expr
constExpr = Number

zeroExpr :: (Expr a) -> (Expr a)
zeroExpr = ZeroExpr

ifExpr :: (Expr a) -> (Expr a) -> (Expr a) -> (Expr a)
ifExpr = IfExpr
--}
valueOfEnv :: Expr -> Environment -> (ExpVal, Environment)
valueOfEnv (Number n) env = (int2Val n, env)
valueOfEnv (IdExpr var) env  = (denVal2ExpVal (apply_environment env var), env)
valueOfEnv (DiffExpr ex1 ex2) env = 
    let 
        (v1, env') = valueOfEnv ex1 env
        (v2, env'') = valueOfEnv ex2 env'
        expval1 = expVal2Int v1
        expval2 = expVal2Int v2
    in
        (int2Val (expval1 - expval2), env'')


valueOfEnv (AddExpr ex1 ex2) env = 
    let 
        (v1, env') = valueOfEnv ex1 env
        (v2, env'') = valueOfEnv ex2 env'
        expval1 = expVal2Int v1
        expval2 = expVal2Int v2
    in
        (int2Val (expval1 + expval2), env'')



valueOfEnv (ZeroExpr ex) env = 
    let 
        (v, env') = valueOfEnv ex env
        x = expVal2Int v
    in
        if x == 0 then (ExpBool True, env') else (ExpBool False, env')

valueOfEnv (IfExpr ex1 ex2 ex3) env =
    let 
        (v, env') = valueOfEnv ex1 env
        x = expVal2Bool v
    in
       if x then valueOfEnv ex2 env' else valueOfEnv ex3 env'

valueOfEnv (LetExpr (IdExpr var) ex1 ex2) env = 
    let 
        (x, env') = valueOfEnv ex1 env 
        y = expVal2DenVal x
        new_env  = extend_environment var y env -- we should not use new env' here. 
                                                -- because env' is only for ex1
                                                -- what i have learnt? env is
                                                -- local to expression. if 2
                                                -- expresssion have connection,
                                                -- then use new env'
    in
        valueOfEnv ex2 new_env

valueOfEnv (NegExpr expr) env = 
    let 
        (x,env') = valueOfEnv expr env
    in
        (ExpInt (-(expVal2Int x)) , env')

-- we do not evaluate Proc, we save it, 
-- only evaluate proc when call it.
valueOfEnv (ProcExpr id expr) env = 
    let 
        x = ExpProc id expr env
        new_env = extend_environment id (expVal2DenVal x) env
    in
       (x, new_env) 

-- let f = proc (x) proc (y) -(x,y)
-- in ((f 3) 4)
--
-- x bind to 3, 4 bind to y , then evaluate -(x,y) 
valueOfEnv (CallExpr ex1 ex2) env = 
    let 
        (proc, env')  = valueOfEnv ex1 env
        (arg, env'')  = valueOfEnv ex2 env'
        (result, env''') = applyProc proc arg
    in
        (result, env'') 


applyProc :: ExpVal -> ExpVal -> (ExpVal, Environment)
applyProc ex1 ex2 = 
  case ex1 of
      (ExpProc id body oldenv) -> let 
                                   env'' = extend_environment id (expVal2DenVal ex2) oldenv
                                   (v, env''') = valueOfEnv body env''
                                   in
                                       (v, env''')
      _                -> error "applyProc missing ExpProc."

-- what i have learnt, extractor , evaluating results from extractor.
-- man, look at Exercise 1.26, (up lst), same as the interpreter here.
-- exracting first element, then evaluate the result from extractin process.
-- look at above, since haskell has pattern match mechanism , so extracting
-- process is just pattern match.
--
-- question, use the extractor, evaluation pattern to build a length function
-- which calculate length of a list.
--
-- like:
--     evaluate length [1,2,3,4,5]
parse :: [Token] -> (Expr, [Token])
parse [] = (ExprEmpty, [])
parse (x:xs) = case x of
    (TNumber n)         -> ((Number (toInteger n)), xs)
    (TIdent  s)         -> parseId (x:xs)
    TAssign             -> parseAssign (x:xs)
    TMinus              -> parseMinus (x:xs)
    TAdd                -> parseAdd (x:xs)
    TIf                 -> parseIf (x:xs) 
    TLet                -> parseLet (x:xs)
    TR                  -> parse xs
    TL                  -> parseTL (x:xs)
    TNeg                -> parseNeg (x:xs)
    TProc               -> parseProc (x:xs)
    TZero               -> parseZero (x:xs)
    _                   -> (ExprEmpty, (x:xs))


parseZero :: [Token] -> (Expr, [Token])
parseZero [] = error "parseZero is zero"
parseZero (TZero:xs) = let (ex, rest) = parse xs
                       in
                           (ZeroExpr ex, rest)

parseProc :: [Token] -> (Expr, [Token])
parseProc (x:xs) = case x of 
                       TProc -> parseProc' xs
                       _     -> error $ "missing proc keyword. instead of " ++ (show x)

parseProc' :: [Token] -> (Expr, [Token])
parseProc' (TL: rest) = 
    let (parameter, rest') = parse rest
    in  
        let (body, rest''') = case rest' of
                                (TR: rest'') -> parse rest''
                                _            -> error "missing ) symbol in proc's paramter list"
        in
            case parameter of
                          (IdExpr id) -> ((ProcExpr id body) , rest''')
                          _          -> error "in parseProc, id is not a IdExpr."

parseTL :: [Token] -> (Expr , [Token])
parseTL [] = error "ParseTL is empty"
parseTL (TL:TL:xs) = parseCall (TL:xs)
parseTL (TL:xs) = 
    let (exp, rest) = parse xs
    in
        case exp of
            (IdExpr s) -> case rest of
                               (TR:rest') -> parse rest' -- id followed by a ). mean, form like (id)
                               _          -> parseCall xs -- id not followed by a ), mean a call, form like this (f 7)
            (Number n) -> case rest of
                               (TR:rest'') -> (exp, rest) -- (4)
                               _           -> error "missing ) symbol, in Number expression"
            _          -> (exp, rest)
              
parseCall :: [Token] -> (Expr , [Token])
parseCall [] = error "parseCall is empty."
parseCall xs = let
        (first, tok) = parse xs
        (second,tok') = parse tok
    in
       case tok' of
           (TR:rest) -> (CallExpr first second, rest)
           _         -> error "CallExpr missing ) symbol."

parseNeg :: [Token] -> (Expr , [Token])
parseNeg [] = error "parseNeg needs (minus) token."
parseNeg (TNeg:TL:xs) = 
    let 
        (exp, rest)= parse xs
    in
        case rest of
            (TR: rest') ->  (NegExpr exp, rest' )
            _           ->  error "minus expression missing ) symbol."

parseAssign :: [Token] -> (Expr , [Token])
parseAssign (x:xs) = case x of
    TAssign -> parse xs
    _         -> error ("parseAssign needs a (=) symbol. instead of " ++ (show x))

parseId :: [Token] -> (Expr , [Token])
parseId (x:xs) = case x of 
                 (TIdent s) -> (IdExpr s, xs)
                 _          -> error ("parseId needs a id. instead of " ++ (show x))

parseLet :: [Token] -> (Expr , [Token])
parseLet [] = (ExprEmpty, [])
parseLet (TLet:rest ) = 
    let (id, toks) = parseLetId rest
        (exp, toks') = parseLetExp toks
        (body, toks'') = parseLetBody toks'
    in
        (LetExpr id exp body, toks'')        

parseLetId :: [Token] -> (Expr , [Token])
parseLetId  = parseId 
       

parseLetExp :: [Token] -> (Expr , [Token])
parseLetExp (x:xs) = case x of 
    TAssign -> parse xs
    _       -> error ("Let need an assignment symbol. instead of " ++ (show x))

parseLetBody :: [Token] -> (Expr , [Token])
parseLetBody (x:xs) = case x of
    TIn -> parse xs
    _   -> error ("Let need an in keyword. instead of " ++ (show x)) 


parseMinus :: [Token] -> (Expr , [Token])
parseMinus [] = error "need a Minus(-) token here"
parseMinus (TMinus:TL:TNumber n:TR:rest) = ((Number (toInteger (-n) )), rest) -- -(4)
parseMinus (TMinus:TNumber n: rest)      = ((Number (toInteger (-n) )), rest) -- -4
parseMinus toks                 = parseDiff toks      -- -(z, j)

parseAdd :: [Token] -> (Expr , [Token])
parseAdd [] = error "Need a Addtion(+) token here."
parseAdd (TAdd:TL:TNumber n:TR:rest) = ((Number (toInteger n) ), rest)
parseAdd (TAdd:TNumber n: rest)      = ((Number (toInteger n) ), rest)
parseAdd toks    = parseAddition toks

parseAddition :: [Token] -> (Expr , [Token])
parseAddition [] = error "parseAddition is empty"
parseAddition (TAdd:TL:xs) = 
    let 
        (first, rest) = parse xs
    in
        case rest of 
            (TCommas:ys) -> let (first', second) = parse ys
                            in
                                case second of
                                     (TR: trafter) -> (AddExpr first first', trafter)
                                     _             -> error "Add expression missing ')' symbol"
            _            -> error "Add expression missing ',' symbol"
                   


parseDiff :: [Token] -> (Expr  , [Token])
parseDiff [] = error "parseDiff is empty" 
parseDiff (TMinus:TL:xs) = let 
       (first, rest) = parse xs
    in 
         case rest of 
              (TCommas:ys) -> let (first', second) = parse ys
                              in
                                  case second of
                                      (TR: trafter) -> (DiffExpr first first', trafter)
                                      _             -> error "Diff expression missing ')' symbol"
              _            -> error $ "Diff expression missing ',' symbol. instead of " ++ (show (head rest))
      
            

parseDiffExpr :: [Token] -> (Expr , [Token])
parseDiffExpr [] = error "Diff expression is empty"
parseDiffExpr xs = 
    let 
        (first, rest) = parse xs
        (second, rest') = parse rest
    in
        ((DiffExpr first second) , rest')

parseIf :: [Token] -> (Expr, [Token])
parseIf [] = error "need a if expression"
parseIf (TIf: xs) = 
    let 
        (condition, rest) = parse xs
        (thenPart, rest') = parseIfThen rest
        (elsePart, rest'') = parseIfElse rest'
    in 
        (IfExpr condition thenPart  elsePart , rest'')


parseIfThen :: [Token] -> (Expr, [Token])
parseIfThen (x:xs) = case x of 
    TThen -> parse xs
    _     -> error "If expression missing Then keyword"

parseIfElse :: [Token] -> (Expr, [Token])
parseIfElse (x:xs) = case x of
    TElse -> parse xs
    _    -> error "If expression missing Else keyword"

data Token = TNumber Int | TIdent String
    | TAssign | TMinus | TAdd | TMul | TDiv | TCons
    | TIn | TLet | TZero | TEqualNum | TGreaterNum | TLessNum
    | TIf | TThen | TElse | TR | TL | TCommas
    | TNeg | TProc | TLetRec
    | TEmpty | TEmptyList deriving (Show , Eq)

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
                 | x == '/' = TDiv : tokenizer xs
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
        "equalNum?" -> TEqualNum
        "greaterNum?" -> TGreaterNum
        "lessNum?" -> TLessNum
        "cons" -> TCons
        "emptylist" -> TEmptyList
        "proc" -> TProc
        "letrec" -> TLetRec
        "minus"  -> TNeg
        _      -> TIdent (x:xs)
    | otherwise = error "wrong token"


