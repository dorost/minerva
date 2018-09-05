{-# LANGUAGE OverloadedStrings #-}

module Inference where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import AST

-- From http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.65.7733&rep=rep1&type=pdf
-- With some small changes

type Subst = Map.Map Text.Text Type

class Types a where
    ftv :: a -> Set.Set Text.Text
    apply :: Subst -> a -> a

data Scheme = Scheme [Text.Text] Type deriving Show

instance Types Type where
    ftv (TVar n) = Set.singleton n
    ftv (Basic _) = Set.empty
    ftv (TFun t1 t2) = ftv t1 `Set.union` ftv t2

    apply s t1@(TVar n) = case Map.lookup n s of
        Nothing -> t1
        Just t -> t
    apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2 )
    apply s t = t

instance Types Scheme where
    ftv (Scheme vars t) = (ftv t) Set.\\ (Set.fromList vars)
    apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)

instance Types a => Types [a] where
    apply s = map (apply s)
    ftv l = foldr Set.union Set.empty (map ftv l)

nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = (Map.map (apply s1 ) s2 ) `Map.union` s1

newtype TypeEnv = TypeEnv (Map.Map Text.Text Scheme)

remove :: TypeEnv -> Text.Text -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

instance Types TypeEnv where
    ftv (TypeEnv env) = ftv (Map.elems env)
    apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
    where vars = Set.toList ((ftv t) Set.\\ (ftv env))

data TIEnv = TIEnv{ }

data TIState = TIState {tiSupply :: Int,
                        tiSubst :: Subst }
type TI a = ErrorT String (ReaderT TIEnv (StateT TIState IO)) a
runTI :: TI a -> IO (Either String a, TIState)
runTI t = do
    (res, st) <- runStateT (runReaderT (runErrorT t) initTIEnv) initTIState
    return (res, st)
    where initTIEnv = TIEnv{ }
          initTIState = TIState{tiSupply = 0,
          tiSubst = Map.empty }

newTyVar :: Text.Text -> TI Type
newTyVar prefix = do
    s <- get
    put s {tiSupply = tiSupply s + 1}
    return (TVar (prefix <> Text.pack (show (tiSupply s))))

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
    nvars <- mapM (\_ -> newTyVar "a") vars
    let s = Map.fromList (zip vars nvars)
    return $ apply s t

mgu :: Type -> Type -> TI Subst
mgu (TFun l r ) (TFun l' r') = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    return (s1 `composeSubst` s2)
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu t1@(Basic x) t2@(Basic y) | x == y = return nullSubst
                        | otherwise = throwError $ "types do not unify: " ++ show t1 ++
                       " vs. " ++ show t2
mgu t1 t2 =
    throwError $ "types do not unify: " ++ show t1 ++" vs. " ++ show t2

varBind :: Text.Text -> Type -> TI Subst
varBind u t 
    | t == TVar u = return nullSubst
    | u `Set.member` ftv t = throwError $ "occur check fails: " ++ show u ++ " vs. " ++ show t
    | otherwise = return (Map.singleton u t)


ti :: TypeEnv -> Expr -> TI (Subst, Type)
ti (TypeEnv env) (Var n) =
    case Map.lookup n env of
    Nothing -> throwError $ "unbound variable: " ++ show n
    Just sigma -> do
        t <- instantiate sigma
        return (nullSubst, t)
ti env (Lam n e) = do
    tv <- newTyVar "a"
    let TypeEnv env' = remove env n
        env'' = TypeEnv (env' `Map.union` (Map.singleton n (Scheme [] tv)))
    (s1 , t1) <- ti env'' e
    return (s1 , TFun (apply s1 tv) t1)
ti env (App e1 e2) = do
    tv <- newTyVar "a"
    (s1 , t1) <- ti env e1
    (s2 , t2) <- ti (apply s1 env) e2
    s3 <- mgu (apply s2 t1) (TFun t2 tv)
    return (s3 `composeSubst` s2 `composeSubst` s1 , apply s3 tv)

typeInference :: Map.Map Text.Text Scheme -> Expr -> TI Type
typeInference env e = do
    (s, t) <- ti (TypeEnv env) e
    return (apply s t)