module AST where

import Data.Text

data Expr
    = App Text Expr
    | Const Text
    deriving Show


-- TODO parametrized type constructors / higher kinded types
data Type =
    Type [Text]
    deriving (Show, Eq)

data TopLevel
    = TypeDef Text [TypeConstructor]
    | FunDef Text Type
    | FunDecl Text [Text] Expr
    deriving Show

type Minerva = [TopLevel]

data TypeConstructor
    = TypeConstructor Text
    deriving Show