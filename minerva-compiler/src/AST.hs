{-# LANGUAGE OverloadedStrings #-}

module AST where

import Data.Text

data Expr
    = App Expr Expr
    | Var Text
    | Tag Text [Expr]
    | Lam Text Expr
    | Bind Text Expr
    | Match Expr [(Pattern, Expr)]
    | Hole
    -- Definitions
    | TypeDef Text [TypeConstructor]
    | FunDef Text Type
    | FunDecl Text [Text] Expr
    deriving Show


toLambda :: [Text] -> Expr -> Expr
toLambda [] e = e
toLambda (a:args) e = Lam a (toLambda args e)

defToLambda :: Expr -> Expr
defToLambda (FunDecl funId args e) =
    Bind funId (toLambda args e)
defToLambda e = e

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
    | TFun Type Type
    | TVar Text
    deriving (Show, Eq)

prettyPrintType :: Type -> Text
prettyPrintType (TVar t) =
    t
prettyPrintType (Basic t) =
    t
prettyPrintType (TFun t u) =
    prettyPrintType t <> " -> " <> prettyPrintType u

type Minerva = [Expr]

data TypeConstructor
    = TypeConstructor Text Type
    deriving Show