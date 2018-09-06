{-# LANGUAGE OverloadedStrings #-}

module Type where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text(Text(..))
import qualified Data.Text as Text
import AST
import Data.Maybe
import Data.List
import Inference
import Data.List (nub)

getLast :: Type -> Text
getLast (Basic x) = x
getLast (TFun t1 t2) = getLast t2

getType:: Expr -> Maybe [(Text, Type)]
getType t = 
    case t of 
        TypeDef t ts ->
                Just [(t, Basic t)]
        _ -> Nothing

getTypeDef :: Expr -> Maybe [(Text, Type)]
getTypeDef t = 
    case t of 
        TypeDef t ts ->
                Just (map (\(TypeConstructor cns u) ->     
                    if t == getLast u
                        then (cns, u)
                        else error (show $ "Last argument of type constructor should be of type" <> t)
                    ) ts)
        _ -> Nothing

getFunDef :: Expr -> Maybe [(Text, Type)]
getFunDef t =
    case t of
        FunDef t ts -> Just [(t, ts)]
        _ -> Nothing

addToMap :: (Text,  Type) -> Map.Map Text Type -> Map.Map Text Type
addToMap (typeRef, ty) m = 
    if Map.member typeRef m
        then error ("Already defined " <> show typeRef)
        else Map.insert typeRef ty m

getTypeDefs :: Minerva -> Map.Map Text Type
getTypeDefs =
    Prelude.foldr addToMap Map.empty . concat . mapMaybe getTypeDef

getTypes :: Minerva -> Map.Map Text Type
getTypes =
    Prelude.foldr addToMap Map.empty . concat . mapMaybe getType
    
addToMap' :: (Text,  Type) -> Map.Map Text Scheme -> Map.Map Text Scheme
addToMap' (typeRef, ty) m = 
    if Map.member typeRef m
        then error ("Already defined " <> show typeRef)
        else Map.insert typeRef (Scheme [] ty) m
    
getTypeDefs' :: Minerva -> Map.Map Text.Text Scheme
getTypeDefs' m =
    let 
        types = Prelude.foldr addToMap' Map.empty (concat (mapMaybe getType m)) 
        typeDefs = Prelude.foldr addToMap' Map.empty (concat (mapMaybe getTypeDef m))
        funDefs = Prelude.foldr addToMap' Map.empty (concat (mapMaybe getFunDef m))
        funDefs' = fmap (unboundedToVar types) funDefs
    in 
        typeDefs `Map.union` funDefs'

unbounded :: Map.Map Text.Text Scheme -> Type -> Maybe Text
unbounded env (Basic x) = 
    if not (Map.member x env) then
        Just x
    else Nothing
unbounded _ _ = Nothing

toVars :: [Text] -> Type -> Type
toVars (x:xs) t@(Basic y) = if x == y then TVar y else (toVars xs t)
toVars xs (TFun t1 t2) = TFun (toVars xs t1) (toVars xs t2) 
toVars xs t = t

unboundedToVar :: Map.Map Text.Text Scheme -> Scheme -> Scheme
unboundedToVar env (Scheme vars t) =
    let x = catMaybes $ map (unbounded env) (toList t)
    
    in Scheme (nub $ vars ++ x) (toVars x t)

checkType' :: Map.Map Text Scheme -> Expr -> Maybe (Either Text Type, TIState)
checkType' env expr = do
    runTI (typeInference env expr)
    --return (res, s)

checkTypes' :: Map.Map Text Scheme -> Minerva -> Maybe Text.Text
checkTypes' env ((Bind bId expr):ms) = do
    (res, _) <- runTI (typeInference env expr)
    case res of
        Left err -> Just err
        Right t -> checkTypes' env ms
checkTypes' env (_:ms) =
    checkTypes' env ms
checkTypes' env [] = Nothing