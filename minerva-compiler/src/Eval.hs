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

getToplevel expr@(FunDef name _) = 
    Just [(name, expr)]
getToplevel expr@(FunDecl name _ _) = 
    Just [(name, expr)]
getToplevel (Bind name expr) = 
    Just [(name, expr)]
getTopLevel _ = Nothing

loadProgram :: [Expr] -> Map.Map Text Expr
loadProgram = Map.fromList . Prelude.concat . mapMaybe getToplevel

eval :: Expr -> Map.Map Text Expr -> Either Text (Expr, Map.Map Text Expr)
eval (Var x) env =
    maybe (Left ("Var not found: " <> x <> pack (show env))) (\e -> eval e env) (Map.lookup x env)
eval (Tag x es) env =
    case (partitionEithers $ Prelude.map (`eval` env) es) of
        ([], esn) -> return ((Tag x (Prelude.map fst esn)), env)
        (e:es, _) -> Left e
eval (FunDecl _ [] e2) env = eval e2 env
eval e@(FunDecl _ _ _) env = return (e, env)
eval (App expr1 e2) env = do
    (e1, nE) <- eval expr1 env
    case e1 of
        FunDecl name (b:bs) expr -> do
            (e3,_) <- eval e2 env
            let nEnv = Map.insert b e3 nE
                nExpr = FunDecl name bs expr
            eval nExpr nEnv
        Lam b expr -> do
            (e3,_) <- eval e2 env
            let nEnv = Map.insert b e3 nE
                nExpr = expr
            eval nExpr nEnv
        Tag name xs ->
            eval (Tag name (e2:xs)) env
        _ ->
            Left "Could not be applied"
eval (Match mExpr ps) env = do
    (Tag tag1 vs, _) <- eval mExpr env
    let ((Pattern _ bs, e):_) = (Prelude.filter (\((Pattern tag2 bs), _) -> tag1 == tag2) ps)
    let nEnv = Prelude.foldr (\(b, e2) ex -> Map.insert b e2 ex) env (Prelude.zip bs (Prelude.reverse vs))
    eval e nEnv
eval expr@(Lam e x) env = 
    return (expr, env)
eval Hole env = 
    return (Hole, env)
    
eval x _ = Left ("Not supported: " <> pack (show x))