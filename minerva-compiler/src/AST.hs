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

data Type 
    = Basic Text
    | To Type Type
    deriving (Show, Eq)

prettyPrintType :: Type -> Text
prettyPrintType (Basic t) =
    t
prettyPrintType (To t u) =
    prettyPrintType t <> " -> " <> prettyPrintType u

type Minerva = [Expr]

data TypeConstructor
    = TypeConstructor Text Type
    deriving Show