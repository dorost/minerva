{-# LANGUAGE OverloadedStrings #-}

module Parser where

import AST
import Data.Text
import Text.Megaparsec
import Text.Megaparsec.Char


type Parser = Parsec () Text

skipSpacing :: Parser Text
skipSpacing =
    takeWhileP Nothing (\c -> c == ' ' || c == '\n')

skipSpacing1 :: Parser Text
skipSpacing1 =
    takeWhile1P Nothing (\c -> c == ' ' || c == '\n')

noSpacing :: Parser Text
noSpacing =
    takeWhile1P Nothing (\c -> c /= ' ' && c /= '\n')

typeConstructor :: Parser TypeConstructor
typeConstructor = do
    -- TODO add type definitions / type application
    skipSpacing
    instanceName <- noSpacing
    skipSpacing
    return (TypeConstructor instanceName)

expr :: Maybe Expr -> Parser Expr
expr Nothing = do
    ref <- takeWhile1P Nothing (\c -> c /= ' ' && c /= '}' )
    skipSpacing
    let e = (Var ref)
    try (expr (Just e)) <|> return e
expr (Just e) = do
    skipSpacing
    ref <- takeWhile1P Nothing (\c -> c /= ' ' && c /= '}' )
    let app = App e (Var ref)

    try (expr (Just app)) <|> return app


typeDef :: Parser Expr
typeDef = do
    typeName <- noSpacing
    skipSpacing1
    string "="
    skipSpacing1
    char '{'
    typeConstructors <- typeConstructor `sepBy` char ','
    char '}'
    return (TypeDef typeName typeConstructors)

funType :: Parser Text
funType = do
    skipSpacing 
    typeName <- noSpacing
    skipSpacing
    return typeName

funDef :: Parser Expr
funDef = do
    funName <- noSpacing
    skipSpacing1
    char ':'
    skipSpacing
    typeArgs <- sepBy funType (string "->")
    skipSpacing
    return (FunDef funName (Type typeArgs))

funArg :: Parser Text
funArg = do
    funArg <- takeWhile1P Nothing (\c -> c /= ' ' && c /= '\n' && c /= '{')
    skipSpacing
    return funArg
    
funDecl :: Parser Expr
funDecl = do
    funName <- noSpacing
    skipSpacing
    args <- many funArg
    char '{'
    skipSpacing
    e <- expr Nothing
    skipSpacing
    char '}'
    return (FunDecl funName args e)

topLevel :: Parser Expr
topLevel = do
    try typeDef <|> try funDef <|> funDecl

parser :: Parser Minerva
parser =
    topLevel `sepBy` skipSpacing