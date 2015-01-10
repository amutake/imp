module Imp.Env where

import Prelude hiding (lookup)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M

import Imp.Syntax

data Value = NumberVal Double
           | StringVal String
           | BoolVal Bool
           | Closure Env [Id] [Statement]
           | Undefined

instance Show Value where
    show (NumberVal num) = show num
    show (StringVal str) = str
    show (BoolVal True) = "true"
    show (BoolVal False) = "false"
    show (Closure _ _ _) = "<closure>"
    show Undefined = "<undefined>"

type Env = IORef EnvList

data EnvList = Top (Map Id Value)
             | Cons (Map Id Value) Env

declare :: Env -> Id -> Value -> IO ()
declare env ident value = do
    envList <- readIORef env
    case envList of
        Top m -> writeIORef env (Top (M.insert ident value m))
        Cons m e -> writeIORef env (Cons (M.insert ident value m) e)



lookup :: Env -> Id -> IO Value
lookup env ident = do
    envList <- readIORef env
    case envList of
        Top m -> maybe (return Undefined) return $ M.lookup ident m
        Cons m e -> maybe (lookup e ident) return $ M.lookup ident m

assign :: Env -> Id -> Value -> IO ()
assign env ident value = do
    envList <- readIORef env
    case envList of
        Top m | M.member ident m -> writeIORef env (Top (M.insert ident value m))
        Top m -> case ident of
                     MkId str -> error $ "non declared identifier '" ++ str ++ "'"
        Cons m e | M.member ident m -> writeIORef env (Cons (M.insert ident value m) e)
        Cons m e -> assign e ident value

consNew :: Env -> IO Env
consNew = newIORef . Cons M.empty

consNewFun :: Env -> [(Id, Value)] -> IO Env
consNewFun env pairs = newIORef (Cons (M.fromList pairs) env)

initialEnv :: IO Env
initialEnv = newIORef (Top M.empty)
