{-# LANGUAGE DeriveDataTypeable #-}
module M9 where

import Data.Typeable.Internal
import Text.ParserCombinators.Parsec
import Data.Maybe
import Data.Map
import Control.Exception
import Prelude hiding (lookup)

type Env = Map String Expression

data Expression = SExpr [Expression]
                | Number Integer
                | Sym String
                | Boolean Bool
                | Fn String ([Expression] -> Env -> IO (Expression))


--------------------------------------------------------------------
-- Introduce Show

instance Show Expression where
  show (Sym a) = a
  show (Number a) = show a
  show (Fn s b) = "*FN:" ++ s ++ "*"
  show (Boolean b) = show b
  show (SExpr (h:t)) = "(" ++
                       show(h) ++
                       (Prelude.foldl (\ start exp -> (start ++ " " ++ show(exp))) "" t) ++ 
                       ")"
  show (SExpr []) = "()"



data LispError = SyntaxError
               | InvalidVar
               | InvalidArgs
               | InvalidFunctionDef
               | InvalidMacroDef
                 deriving (Show, Typeable)

instance Exception LispError


number :: Parser Expression
number = fmap (Number . read) $ many1 $ oneOf "1234567890"


symbol :: Parser Expression
symbol = fmap Sym $ many1 $ oneOf "&abcdefghijklmnopqrstuvwxyz_-+*?"


program :: Parser [Expression]
program =
  do first <- expression
     next <- remainingExpressions
     return (first : next)

remainingExpressions :: Parser [Expression]
remainingExpressions =
    (oneOf " ,\n" >> program)
    <|> return []


sexp :: Parser Expression
sexp = do
    char '('
    exp <- program
    char ')'
    return $ SExpr exp


expression :: Parser Expression
expression = sexp 
    <|> number
    <|> symbol


parseExpr :: String -> Either ParseError Expression
parseExpr = parse expression "(unknown)"


eval :: Expression -> Env -> IO (Expression)
eval (Number n) env = return $ Number n
eval (SExpr e) env = evalSexpr e env
eval (Sym s) env = lookupEnv s env
eval _ _ = throwIO SyntaxError


evalSexpr :: [Expression] -> Env -> IO (Expression)
evalSexpr (es:t) env = do
   s <- eval es env
   applyFn s t env
evalSexpr _ _ = throwIO SyntaxError


runEval :: Either ParseError Expression -> Env -> IO (Expression)
runEval (Right x) env = eval x env
runEval (Left x) env = throwIO SyntaxError


---------------------------------------------------------------------
-- Introduce Enviroment
 
initEnv :: Env
initEnv = fromList [("true", (Boolean True)),
                    ("false", (Boolean False)),
                    ("+", (Fn "+" (stdFn lispAdd))),
                    ("fn", (Fn "fn" lispFn)),
                    ("macro", (Fn "macro" lispMacro)),
                    ("cons", (Fn "cons" (stdFn lispCons))),
                    ("first", (Fn "first" (stdFn lispFirst))),
                    ("rest", (Fn "rest" (stdFn lispRest))),
                    ("empty?", (Fn "empty?" (stdFn lispEmpty))),
                    ("if", (Fn "if" lispIf)),
                    ("list", (Fn "list" (stdFn lispList))),
                    ("quote", (Fn "quote" lispQuote))]

lookupEnv :: String -> Env -> IO (Expression)
lookupEnv sym env = 
  case (lookup sym env) of
      Just x -> return x
      Nothing -> throwIO InvalidVar 


---------------------------------------------------------------------
-- Introduce Fn Processing

stdFn :: ([Expression] -> Env -> IO (Expression)) -> ([Expression] -> Env -> IO (Expression))
stdFn fn = (\exps env -> do
               nexps <- sequence $ fmap (\x -> eval x env) exps
               fn nexps env)

                   
lispAdd :: [Expression] -> Env -> IO (Expression)
lispAdd (x:[]) env = eval x env
lispAdd (e:t) env = do
  let (Number x) = e 
  (Number rst) <- lispAdd t env
  return $ Number $ x + rst
lispAdd _ _ = throwIO InvalidArgs


lispList ::[Expression] -> Env -> IO (Expression)
lispList col env = return (SExpr col)
--lispList _ _ = throwIO InvalidArgs


lispCons :: [Expression] -> Env -> IO (Expression)
lispCons (el:(SExpr col):[]) env = return (SExpr (el:col))
lispCons _ _ = throwIO InvalidArgs


lispFirst :: [Expression] -> Env -> IO (Expression)
lispFirst ((SExpr (x:_)):[]) env = return x
lispFirst _ _ = throwIO InvalidArgs


lispRest :: [Expression] -> Env -> IO (Expression)
lispRest ((SExpr (_:r)):[]) env = return (SExpr r)
lispRest _ _ = throwIO InvalidArgs


lispEmpty :: [Expression] -> Env -> IO (Expression)
lispEmpty ((SExpr []):[]) env = return $ Boolean True
lispEmpty ((SExpr (x:y)):[]) env = return $ Boolean False
lispEmpty _ _ = throwIO InvalidArgs


lispIf :: [Expression] -> Env -> IO (Expression)
lispIf (pred:t:f:[]) env = do
  (Boolean tst) <- eval pred env
  if tst
    then eval t env
    else eval f env
lispIf _ _ = throwIO InvalidArgs


lispQuote :: [Expression] -> Env -> IO (Expression)
lispQuote (expr:[]) env = return expr


applyFn :: Expression -> [Expression] -> Env -> IO (Expression)
applyFn (Fn s f) args env = do
  f args env


evalBody :: [Expression] -> Env -> IO(Expression)
evalBody body fnEnv = do
  x <- sequence $ fmap (\x -> eval x fnEnv) body
  return (last x)


lispFn :: [Expression] -> Env -> IO (Expression)
lispFn ((SExpr args):body) env = lispFn ((Sym "_"):(SExpr args):body) env 
lispFn ((Sym s):rst) env = do
  let ((SExpr args):body) = rst 
  return (Fn s (\ params callEnv -> do
                   fnEnv <- mergeFnArgs args params callEnv env
                   recursiveFn <- (lispFn ((Sym s):rst) env)
                   let nEnv = (insert s recursiveFn fnEnv)
                   res <- evalBody body nEnv 
                   return res)) 
lispFn (h:t) _ = throwIO InvalidFunctionDef


mergeFnArgs :: [Expression] -> [Expression] -> Env -> Env -> IO Env
mergeFnArgs [] [] callEnv fnEnv = return fnEnv 
mergeFnArgs (h:t) [] _ _ = throwIO InvalidFunctionDef
mergeFnArgs [] (h:t) _ _ = throwIO InvalidFunctionDef
mergeFnArgs ((Sym "&"):(Sym h):[]) exps callEnv fnEnv = do
  vargs <- sequence $ fmap (\x -> eval x fnEnv) exps
  return (insert h (SExpr vargs) fnEnv)
mergeFnArgs ((Sym x):xs) (y:ys) callEnv fnEnv = do
  nexp <- eval y callEnv
  mergeFnArgs xs ys callEnv (insert x nexp fnEnv)


lispMacro :: [Expression] -> Env -> IO (Expression)
lispMacro ((SExpr args):body) env = lispMacro ((Sym "_"):(SExpr args):body) env
lispMacro ((Sym s):rst) env =do
  let ((SExpr args):body) = rst 
  return (Fn s (\ params callEnv -> do 
                  fnEnv <- mergeMacroArgs args params env
                  recursiveFn <- (lispMacro ((Sym s):rst) env)
                  let nEnv = (insert s recursiveFn fnEnv)
                  form <- evalBody body nEnv
                  res <- eval form callEnv
                  return res))
lispMacro (h:t) _ = throwIO InvalidMacroDef


mergeMacroArgs :: [Expression] -> [Expression] -> Env -> IO Env
mergeMacroArgs [] [] fnEnv = return fnEnv 
mergeMacroArgs (h:t) [] _ = throwIO InvalidMacroDef
mergeMacroArgs [] (h:t) _ = throwIO InvalidMacroDef
mergeMacroArgs ((Sym "&"):(Sym h):[]) exps fnEnv = return (insert h (SExpr exps) fnEnv)
mergeMacroArgs ((Sym x):xs) (y:ys) fnEnv = mergeMacroArgs xs ys (insert x y fnEnv)


wrapVar :: String -> String -> String -> String 
wrapVar sym fn body =
  "((fn (" ++ sym ++ ")\n" ++ body ++ ")\n" ++ fn ++ ")"


stdlib =  [("let*", "(macro (plst & body)\n" ++
                    "(cons (cons (quote fn)\n" ++
                    "(cons (cons (first plst) (list))\n" ++
                    "body))\n" ++ 
                    "(rest plst)))"),
           
           ("let", "(macro (plst & body)\n" ++
                   "(if (empty? plst)\n" ++
                   "(cons (cons (quote fn) (cons (list) body)) (list))\n" ++
                   "(cons (quote let*)\n" ++
                   "(cons (cons (first plst)\n" ++ 
                   "(cons (first (rest plst)) (list)))\n" ++
                   "(cons (cons (quote let)\n" ++
                   "(cons (rest (rest plst))\n" ++ 
                   "body))\n"++ 
                   "(list))))))"),
           
          ("map", "(fn map (f c)\n" ++
                  "(if (empty? c)\n" ++
                  "(list)\n" ++
                  "(cons (f (first c)) (map f (rest c)))))"),

          ("reduce", "(fn rd (f a b)\n" ++
                     "(if (empty? b)\n" ++
                     "a\n" ++
                     "(rd f (f a (first b)) (rest b))))")]                 


wrapStdLib :: String -> String
wrapStdLib program = Prelude.foldl (\ p (s, b) -> wrapVar s b p) program stdlib

main :: IO ()
main = do
  let program = wrapStdLib ("(let (x 1 y (quote (1 2 3)) w (map (fn (z) (+ z x)) y))\n" ++
                            "(reduce (fn (a b) (+ a b)) 1 w))")
  let ast = parseExpr $ program
  res <- runEval ast $ initEnv
  putStr $ (show res) ++ "\n"

