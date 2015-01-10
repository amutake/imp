module Imp.Syntax where

type Program = [Statement]

data Statement = Var Id Expr
               | Assign Id Expr
               | If Expr [Statement] [Statement]
               | While Expr [Statement]
               | Return Expr
               | Print Expr
               | Expr Expr

data Expr = Const Const
          | Fun [Id] [Statement]
          | Op Op Expr Expr
          | Apply Expr [Expr]
          | Id Id

data Const = Number Double
           | Boolean Bool
           | String String

data Id = MkId String deriving (Eq, Ord)

data Op = Eq | Neq
        | And | Or
        | Add | Sub | Mul | Div
        | Lt | Le | Gt | Ge
        | App

instance Show Op where
    show Eq = "=="
    show Neq = "!="
    show And = "&&"
    show Or = "||"
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Lt = "<"
    show Le = "<="
    show Gt = ">"
    show Ge = ">="
    show App = "++"
