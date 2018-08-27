{-# LANGUAGE OverloadedStrings #-}

module Type where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text(Text(..))
import AST
import Data.Maybe

--data Type = TyConst

--type TypeEnv = Map Text Text


--checkType :: AST -> TypeEnv -> TypeEnv
--checkType ast env =
--    case ast of
--        TypeDef ref cons ->
--            if ref `Map.elem` env
--                then
--                    error "Already defined " <> ref
--                else  
--                    foldr ()


getTypeLiteral :: TypeOrFunDef -> Maybe Text
getTypeLiteral t = 
    case t of 
        TypeDef t _ -> Just t
        _ -> Nothing

getTypeConstructor :: TypeOrFunDef -> Maybe [(Text, Text)]
getTypeConstructor t = 
    case t of 
        TypeDef t ts -> Just (map (\(TypeConstructor x) -> (t,x)) ts)
        _ -> Nothing
        
        
-- TODO parametrized type constructors / higher kinded types
data Type =
    TyName Text
    deriving Show

data Problem =
    NotEqual Type Type
    deriving Show

addToMap :: (Text,  Text) -> Map.Map Text Type -> Map.Map Text Type
addToMap (ty, tConstr) m = 
    if Map.member tConstr m
        then error "Already defined"
        else Map.insert tConstr (TyName ty) m

getTypeConstructors :: Minerva -> Map.Map Text Type
getTypeConstructors =
    Prelude.foldr addToMap Map.empty . concat . mapMaybe getTypeConstructor

checkType :: Expr -> Map.Map Text Type -> Type
checkType (Const t) env =
    if Map.member t env
        then fromJust $ Map.lookup t env
        else error "Const not found"
checkType _ _ = 
    error "not supported"

-- check expressions
-- Todo complex functions / expressions
checkTypes :: Map.Map Text Type -> Minerva -> Maybe Problem
checkTypes env (FunDef name (FunType ty: _) expr: ms) =
    let TyName tyExpr = checkType expr env
    in
        if tyExpr /= ty
            then Just (NotEqual (TyName tyExpr) (TyName ty))
            else checkTypes env ms
checkTypes env (_:ms) =
    checkTypes env ms
checkTypes env  [] = Nothing