module Eval where

import qualified Data.Map as Map
import Data.Text
import AST


getToplevel :: Expr -> Maybe (Text, Expr)
getToplevel expr@(TypeDef name _) = 
    Just (name, expr)
getToplevel expr@(FunDef name _) = 
    Just (name, expr)
getToplevel expr@(FunDecl name _ _) = 
    Just (name, expr)
getTopLevel _ = Nothing



eval :: Expr -> Map.Map Text Expr -> Expr
eval (Var x) env = 
    eval (env Map.! x) env

eval _ _ = error "Not supported"