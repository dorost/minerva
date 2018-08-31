{-# LANGUAGE OverloadedStrings #-}

module Type where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text(Text(..))
import qualified Data.Text as Text
import AST
import Data.Maybe
import Data.List


getTypeLiteral :: Expr -> Maybe Text
getTypeLiteral t = 
    case t of 
        TypeDef t _ -> Just t
        _ -> Nothing

getTypeDef :: Expr -> Maybe [(Text, Type)]
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
        then error ("Already defined " <> show typeRef)
        else Map.insert typeRef ty m

getTypeDefs :: Minerva -> Map.Map Text Type
getTypeDefs =
    Prelude.foldr addToMap Map.empty . concat . mapMaybe getTypeDef

checkType :: Expr -> Map.Map Text Type -> Type
checkType (Var t) env =
    if Map.member t env
        then fromJust $ Map.lookup t env
        else error ("Var not found " <> show t)
checkType (App t1 t2) env =
    let Type ty1 = checkType t1 env
        Type ty2 = checkType t2 env
        nTy = stripPrefix ty2 ty1
    in
        case nTy of
            Just x -> Type x
            Nothing -> error ("xxxx")

checkType _ _ = 
    error "not supported"

bindNames :: [Text] -> Type -> Map.Map Text Type -> (Map.Map Text Type, Type)
bindNames [] rem env = (env, rem)
bindNames (b: bs) (Type (t: t2: ty)) env =
    bindNames bs (Type (t2: ty)) (Map.insert b (Type [t]) env)
bindNames _ _ _ = error "Function has too many variables"


-- check expressions
-- Todo complex functions / expressions
checkTypes :: Map.Map Text Type -> Minerva -> Maybe Problem
checkTypes env (FunDecl name bs expr: ms) =
        let 
            tyFun =
                case Map.lookup name env of 
                    Just x -> x
                    Nothing -> error (show name)
            (nEvn, nTyFun) = bindNames bs tyFun env
            tyExpr = checkType expr nEvn
    in
        if tyExpr /= nTyFun
            then Just (NotEqual tyExpr nTyFun)
            else checkTypes env ms
checkTypes env (_:ms) =
    checkTypes env ms
checkTypes env  [] = Nothing