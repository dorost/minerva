{-# LANGUAGE OverloadedStrings #-}

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

prettyPrintExpr :: Expr -> Text
prettyPrintExpr (Tag x) = x 
prettyPrintExpr e = pack (show e)
-- TODO parametrized type constructors / higher kinded types
data Type =
    Type [Text]
    deriving (Show, Eq)

prettyPrintType :: Type -> Text
prettyPrintType (Type ts) =
    intercalate " -> " ts
type Minerva = [Expr]

data TypeConstructor
    = TypeConstructor Text
    deriving Show