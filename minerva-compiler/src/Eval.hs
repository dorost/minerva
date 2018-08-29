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
eval x _ = error $ "Not supported" <> show x