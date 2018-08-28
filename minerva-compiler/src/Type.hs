{-# LANGUAGE OverloadedStrings #-}

module Type where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text(Text(..))
import qualified Data.Text as Text
import AST
import Data.Maybe



getTypeLiteral :: TopLevel -> Maybe Text
getTypeLiteral t = 
    case t of 
        TypeDef t _ -> Just t
        _ -> Nothing

getTypeDef :: TopLevel -> Maybe [(Text, Type)]
getTypeDef t = 
    case t of 
        TypeDef t ts -> Just (map (\(TypeConstructor cns) -> (cns, Type [t])) ts)
        FunDef t ts -> Just [(t, ts)]
        _ -> Nothing
        

data Problem =
    NotEqual Type Type
    deriving Show

addToMap :: (Text,  Type) -> Map.Map Text Type -> Map.Map Text Type
addToMap (typeRef, ty) m = 
    if Map.member typeRef m
        then error ("Already defined " <> show ty)
        else Map.insert typeRef ty m

getTypeDefs :: Minerva -> Map.Map Text Type
getTypeDefs =
    Prelude.foldr addToMap Map.empty . concat . mapMaybe getTypeDef

checkType :: Expr -> Map.Map Text Type -> Type
checkType (Const t) env =
    if Map.member t env
        then fromJust $ Map.lookup t env
        else error ("Var not found " <> show t)
checkType _ _ = 
    error "not supported"

-- check expressions
-- Todo complex functions / expressions
checkTypes :: Map.Map Text Type -> Minerva -> Maybe Problem
checkTypes env (FunDecl name bs expr: ms) =
    let tyExpr = checkType expr env
        tyFun = env Map.! name
    in
        if tyExpr /= tyFun
            then Just (NotEqual tyExpr tyFun)
            else checkTypes env ms
checkTypes env (_:ms) =
    checkTypes env ms
checkTypes env  [] = Nothing