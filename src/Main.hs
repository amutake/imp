module Main where

import qualified Data.Text.IO as T

import Imp.Parser
import Imp.Env
import Imp.Eval

main :: IO ()
main = do
    code <- T.getContents
    putStrLn "====== code ======"
    T.putStr code
    putStrLn "====== end  ======"
    case parseCode code of
        Left err -> print err
        Right prog -> do
            putStrLn "finish parsing"
            env <- initialEnv
            run env prog
            return ()
