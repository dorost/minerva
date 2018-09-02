{-# LANGUAGE OverloadedStrings #-}

module Eval where

import qualified Data.Map as Map
import Data.Text
import AST
import Data.Maybe
import Data.Either

getToplevelConstructors :: TypeConstructor -> (Text, Expr)
getToplevelConstructors (TypeConstructor tConst _) =
    (tConst, Tag tConst [])

getToplevel :: Expr -> Maybe [(Text, Expr)]
getToplevel expr@(TypeDef name constrs) =
    Just (Prelude.map getToplevelConstructors constrs)
    -- TODO functions returing types

getToplevel expr@(FunDef name _) = 
    Just [(name, expr)]
getToplevel expr@(FunDecl name _ _) = 
    Just [(name, expr)]
getTopLevel _ = Nothing

loadProgram :: [Expr] -> Map.Map Text Expr
loadProgram = Map.fromList . Prelude.concat . mapMaybe getToplevel

eval :: Expr -> Map.Map Text Expr -> Either Text Expr
eval (Var x) env =
    maybe (Left ("Var not found: " <> x)) (\e -> eval e env) (Map.lookup x env)
eval (Tag x es) env =
    case (partitionEithers $ Prelude.map (`eval` env) es) of
        ([], esn) -> Right (Tag x esn)
        (e, _) -> Left "Something occured in evaluating data type"
eval (FunDecl _ [] e2) env = eval e2 env
eval (FunDecl name bs e2) env = return $ FunDecl name bs e2
eval (App expr1 e2) env = do
    e1 <- eval expr1 env
    case e1 of
        FunDecl name (b:bs) expr ->
            let nEnv = Map.insert b e2 env
            in
                eval (FunDecl name bs expr) nEnv
        Tag name xs ->
            eval (Tag name (e2:xs)) env
        _ ->
            Left "Could not be applied"

eval x _ = Left ("Not supported" <> pack (show x))