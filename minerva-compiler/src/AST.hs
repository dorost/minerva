module AST where

import Data.Text

data Expr
    = App Text Expr
    | Const Text
    deriving Show


-- TODO parametrized type constructors / higher kinded types
data Type =
    TyName Text
    deriving (Show, Eq)


data TypeOrFunDef
    = TypeDef Text [TypeConstructor]
    | FunDef Text [Type] Expr
    deriving Show

type Minerva = [TypeOrFunDef]

data TypeConstructor
    = TypeConstructor Text
    deriving Show