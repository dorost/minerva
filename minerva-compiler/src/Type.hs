{-# LANGUAGE OverloadedStrings #-}

module Type where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text(Text(..))
import AST
import Data.Maybe

--data Type = TyConst

--type TypeEnv = Map Text Text

--isTypeDef :: AST -> Bool
--isTypeDef (TypeDef)

--checkTypeCons :: TypeConstructor -> TypeEnv -> TypeEnv
--checkTypeCons (TypeConstructor typeCons) =

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

addToMap :: (Text,  Text) -> Map.Map Text Type -> Map.Map Text Type
addToMap (ty, tConstr) m = 
    if Map.member tConstr m
        then error "Already defined"
        else Map.insert tConstr (TyName ty) m

getTypeConstructors :: Minerva -> Map.Map Text Type
getTypeConstructors =
    Prelude.foldr addToMap Map.empty . concat . mapMaybe getTypeConstructor

