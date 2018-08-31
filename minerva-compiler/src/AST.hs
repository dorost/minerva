module AST where

import Data.Text

data Expr
    = App Expr Expr
    | Var Text
    | Tag Text 
    | TypeDef Text [TypeConstructor]
    | FunDef Text Type
    | Lam Text Expr
    | Let Text Expr
    | FunDecl Text [Text] Expr
    deriving Show


-- TODO parametrized type constructors / higher kinded types
data Type =
    Type [Text]
    deriving (Show, Eq)

type Minerva = [Expr]

data TypeConstructor
    = TypeConstructor Text
    deriving Show