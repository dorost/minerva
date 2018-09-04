{-# LANGUAGE OverloadedStrings #-}

module AST where

import Data.Text

data Expr
    = App Expr Expr
    | Var Text
    | Tag Text [Expr]
    | Lam Text Expr
    | Let Text Expr
    | Match Expr [(Pattern, Expr)]
    -- Definitions
    | TypeDef Text [TypeConstructor]
    | FunDef Text Type
    | FunDecl Text [Text] Expr
    deriving Show

data Pattern =
    -- Tag + bindings
    Pattern Text [Text]
    deriving Show

prettyPrintExpr :: Expr -> Text
prettyPrintExpr (Tag x []) = x
prettyPrintExpr (Tag x xs) = x <> " (" <> Data.Text.intercalate ") (" (Prelude.map prettyPrintExpr (Prelude.reverse xs)) <> ")"
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