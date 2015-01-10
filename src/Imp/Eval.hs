module Imp.Eval where

import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import Control.Monad (when)

import Imp.Syntax
import Imp.Env

eval :: Env -> Expr -> IO Value
eval _ (Const (Number num)) = return (NumberVal num)
eval _ (Const (String str)) = return (StringVal str)
eval _ (Const (Boolean bool)) = return (BoolVal bool)
eval env (Fun idents body) = return (Closure env idents body)
eval env (Op op e1 e2) = do
    v1 <- eval env e1
    v2 <- eval env e2
    return (opEval op v1 v2)
eval env (Apply expr args) = do
    f <- eval env expr
    case f of
        Closure env' idents body -> do
            when (length idents /= length args) $ error "the number of arguments is wrong"
            args' <- mapM (eval env) args
            funenv <- consNewFun env' (zip idents args')
            result <- run funenv body
            maybe (return Undefined) return result
        _ -> error "it must be function"
eval env (Id id) = lookup env id

opEval :: Op -> Value -> Value -> Value
opEval op value1 value2 = opEval' op value1 value2
  where
    opEval' Eq (NumberVal n1) (NumberVal n2) = BoolVal (n1 == n2)
    opEval' Eq (StringVal s1) (StringVal s2) = BoolVal (s1 == s2)
    opEval' Eq (BoolVal b1) (BoolVal b2) = BoolVal (b1 == b2)
    opEval' Neq v1 v2 = case opEval' Eq v1 v2 of
        BoolVal b -> BoolVal (not b)
        _ -> error "assert false: opEval' Neq"
    opEval' And (BoolVal b1) (BoolVal b2) = BoolVal (b1 && b2)
    opEval' Or (BoolVal b1) (BoolVal b2) = BoolVal (b1 || b2)
    opEval' Add (NumberVal n1) (NumberVal n2) = NumberVal (n1 + n2)
    opEval' Sub (NumberVal n1) (NumberVal n2) = NumberVal (n1 - n2)
    opEval' Mul (NumberVal n1) (NumberVal n2) = NumberVal (n1 * n2)
    opEval' Div (NumberVal n1) (NumberVal n2) = NumberVal (n1 / n2)
    opEval' Lt (NumberVal n1) (NumberVal n2) = BoolVal (n1 < n2)
    opEval' Le (NumberVal n1) (NumberVal n2) = BoolVal (n1 <= n2)
    opEval' Gt (NumberVal n1) (NumberVal n2) = BoolVal (n1 > n2)
    opEval' Ge (NumberVal n1) (NumberVal n2) = BoolVal (n1 >= n2)
    opEval' App (StringVal s1) (StringVal s2) = StringVal (s1 ++ s2)
    opEval _ _ _ = error $ concat
                   [ "wrong type of arguments ("
                   , show value1
                   , " " ++ show op ++ " "
                   , show value2 ++ ")"
                   ]

step :: Env -> Statement -> IO (Maybe Value)
step env (Var ident expr) = do
    value <- eval env expr
    declare env ident value
    return Nothing
step env (Assign ident expr) = do
    value <- eval env expr
    assign env ident value
    return Nothing
step env (If expr trueBlock falseBlock) = do
    cond <- eval env expr
    case cond of
        BoolVal True -> consNew env >>= flip run trueBlock
        BoolVal False -> consNew env >>= flip run falseBlock
        _ -> error "condition of if statement must be evaluated to boolean value"
step env (While expr block) = do
    cond <- eval env expr
    case cond of
        BoolVal True -> do
            maybeReturn <- run env block
            maybe (step env (While expr block)) (return . Just) maybeReturn
        BoolVal False -> return Nothing
        _ -> error "condition of while statement must be evaluated to boolean value"
step env (Return expr) = Just <$> eval env expr
step env (Print expr) = eval env expr >>= print >> return Nothing
step env (Expr expr) = eval env expr >> return Nothing

run :: Env -> Program -> IO (Maybe Value)
run _ [] = return Nothing
run env (stmt : stmts) = do
    result <- step env stmt
    maybe (run env stmts) (return . Just) result