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

getLast :: Type -> Text
getLast (Basic x) = x
getLast (To t u) = getLast u

getTypeDef :: Expr -> Maybe [(Text, Type)]
getTypeDef t = 
    case t of 
        TypeDef t ts ->
                Just (map (\(TypeConstructor cns u) ->     
                    if t == getLast u
                        then (cns, u)
                        else error (show $ "Last argument of type constructor should be of type" <> t)
                    ) ts)
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

stripApp :: Type -> Type -> Maybe Type
stripApp (To x@(Basic t1) t2) y@(Basic t3) =
    if x == y then Just t2 else Nothing
stripApp _ _ =
    Nothing

checkType :: Expr -> Map.Map Text Type -> Type
checkType (Var t) env =
    fromMaybe (error ("Var not found " <> show t)) (Map.lookup t env)
checkType (App t1 t2) env =
    let ty1 = checkType t1 env
        ty2 = checkType t2 env
        nTy = stripApp ty1 ty2
    in
        fromMaybe (error ("unexpected: checkType" <> show ty1 <> show ty2)) nTy
checkType (Tag t) env =
    fromMaybe (error ("Tag not found " <> show t)) (Map.lookup t env)
checkType e _ = 
    error ("not supported" <> show e)

bindNames :: [Text] -> Type -> Map.Map Text Type -> (Map.Map Text Type, Type)
bindNames [] rem env = (env, rem)
bindNames (b: bs) (To t t2) env =
    bindNames bs t2 (Map.insert b t env)
bindNames _ _ _ = error "Function has too many variables"


-- check expressions
-- Todo complex functions / expressions
checkTypes :: Map.Map Text Type -> Minerva -> Maybe Problem
checkTypes env (FunDecl name bs expr: ms) =
        let 
            tyFun =
                fromMaybe (error (show name)) (Map.lookup name env)
            (nEvn, nTyFun) = bindNames bs tyFun env
            tyExpr = checkType expr nEvn
    in
        if tyExpr /= nTyFun
            then Just (NotEqual tyExpr nTyFun)
            else checkTypes env ms
checkTypes env (_:ms) =
    checkTypes env ms
checkTypes env  [] = Nothing