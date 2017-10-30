module Language where

type Name = String

data Expr
  = EVar Name
  | ENum Integer
  | EAp Expr Expr
  deriving Show

type ScDefn = (Name, [Name], Expr)
type Program = [ScDefn]
