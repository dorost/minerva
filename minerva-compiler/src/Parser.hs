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

expr :: Parser Expr
expr = do
    ref <- noSpacing

    return (Const ref)

typeDef :: Parser TopLevel
typeDef = do
    typeName <- noSpacing
    skipSpacing1
    string "="
    skipSpacing1
    char '{'
    typeConstructors <- typeConstructor `sepBy` char ','
    char '}'
    return (TypeDef typeName typeConstructors)

funType :: Parser Type
funType = do
    skipSpacing 
    typeName <- noSpacing
    return (TyName typeName)

funDef :: Parser TopLevel
funDef = do
    funName <- noSpacing
    skipSpacing1
    char ':'
    skipSpacing
    typeArgs <- sepBy funType (string "->")
    skipSpacing1
    return (FunDef funName typeArgs)

funArg :: Parser Text
funArg = do
    funArg <- noSpacing
    return funArg
    
funDecl :: Parser TopLevel
funDecl = do
    funName <- noSpacing
    args <- sepBy funArg (char ' ')
    skipSpacing
    char '='
    skipSpacing
    e <- expr
    skipSpacing
    return (FunDecl funName args e)

topLevel :: Parser TopLevel
topLevel = do
    try typeDef <|> try funDef <|> funDecl

parser :: Parser Minerva
parser =
    topLevel `sepBy` skipSpacing