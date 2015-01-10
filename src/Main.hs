{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import qualified Data.Text.IO as T
import System.Environment
import System.IO

import Imp.Parser
import Imp.Prim
import Imp.Eval

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["repl"] -> repl
        ["help"] -> help
        [filepath] -> readAndRun filepath
        _ -> help

help :: IO ()
help = putStr $ unlines
    [ "Usage: imp [COMMAND or FILEPATH]"
    , ""
    , "Commands:"
    , "  help      show this help text"
    , "  repl      start REPL"
    ]

repl :: IO ()
repl = do
    env <- initialEnv
    loop env
  where
    loop env = do
        putStr "imp> "
        hFlush stdout
        code <- T.getLine
        case parseCode code of
            Left err -> print err >> loop env
            Right prog -> run env prog >> loop env

readAndRun :: FilePath -> IO ()
readAndRun filepath = do
    code <- T.readFile filepath
    putStrLn "======== code ========"
    T.putStr code
    putStrLn "======== end ========="
    case parseCode code of
        Left err -> print err
        Right prog -> do
            putStrLn "=== finish parsing ==="
            putStrLn $ show prog
            putStrLn "======== end ========="
            env <- initialEnv
            run env prog
            return ()
