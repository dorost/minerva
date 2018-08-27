module AST where

import Data.Text

data Expr
    = App Text Expr
    | Const Text
    deriving Show

data FunType
    = FunType Text
    deriving Show

data TypeOrFunDef
    = TypeDef Text [TypeConstructor]
    | FunDef Text [FunType] Expr
    deriving Show

type Minerva = [TypeOrFunDef]

data TypeConstructor
    = TypeConstructor Text
    deriving Show