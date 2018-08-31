module Eval where

import qualified Data.Map as Map
import Data.Text
import AST
import Data.Maybe

getToplevelConstructors :: TypeConstructor -> (Text, Expr)
getToplevelConstructors (TypeConstructor tConst) =
    (tConst, Tag tConst)

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

eval :: Expr -> Map.Map Text Expr -> Expr
eval (Var x) env = 
    eval (env Map.! x) env
eval (Tag x) env = 
    Tag x
eval (FunDecl _ [] e2) env = eval e2 env
eval (FunDecl name bs e2) env = FunDecl name bs e2
eval (App expr1 e2) env =
    case eval expr1 env of
        FunDecl name (b:bs) expr ->
            let nEnv = Map.insert b e2 env
            in
                eval (FunDecl name bs expr) nEnv
        _ ->
            error "Could not be applied"
        

eval x _ = error $ "Not supported" <> show x