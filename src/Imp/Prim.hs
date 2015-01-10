module Imp.Prim where

import Prelude hiding (not, length, print)
import qualified Prelude as P
import Control.Exception (throwIO)
import Data.IORef
import Data.List (intercalate)
import qualified Data.Map as M

import Imp.Syntax
import Imp.Env

initialEnv :: IO Env
initialEnv = newIORef (Top (M.fromList primitives))

primitives :: [(Id, Value)]
primitives = map (\(s, f) -> (MkId s, PrimFunc f))
    [ ("not", not)
    , ("number_to_string", number_to_string)
    , ("boolean_to_string", boolean_to_string)
    , ("number?", is_number)
    , ("string?", is_string)
    , ("boolean?", is_boolean)
    , ("length", length)
    , ("print", print)
    , ("println", println)
    ]
  where
    primError name args = throwIO $ ImpError $ concat
        [ "invalid arguments: "
        , name
        , "(" ++ intercalate "," (map show args) ++ ")"
        ]
    boolean = return . BoolVal
    string = return . StringVal
    number = return . NumberVal

    not [BoolVal b] = boolean (P.not b)
    not args = primError "not" args

    number_to_string [NumberVal n] = string (show n)
    number_to_string args = primError "number_to_string" args

    boolean_to_string [BoolVal b] = string (show (BoolVal b))
    boolean_to_string args = primError "boolean_to_string" args

    is_number [NumberVal _] = boolean True
    is_number [_] = boolean False
    is_number args = primError "number?" args

    is_boolean [BoolVal _] = boolean True
    is_boolean [_] = boolean False
    is_boolean args = primError "boolean?" args

    is_string [StringVal _] = boolean True
    is_string [_] = boolean False
    is_string args = primError "string?" args

    length [StringVal s] = number . fromInteger . toInteger . P.length $ s
    length args = primError "length" args

    print args = do
        putStr $ intercalate "," $ map show args
        return Undefined

    println args = do
        putStrLn $ intercalate "," $ map show args
        return Undefined
