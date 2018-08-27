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

typeDef :: Parser TypeOrFunDef
typeDef = do
    typeName <- noSpacing
    skipSpacing1
    string "="
    skipSpacing1
    char '{'
    typeConstructors <- typeConstructor `sepBy` char ','
    char '}'
    return (TypeDef typeName typeConstructors)

funType :: Parser FunType
funType = do
    -- TODO
    typeName <- noSpacing
    return (FunType typeName)

funDef :: Parser TypeOrFunDef
funDef = do
    funName <- noSpacing
    skipSpacing1
    char ':'
    skipSpacing
    typeArgs <- sepBy funType (string "->")
    skipSpacing1
    char '='
    skipSpacing1
    exp <- expr
    skipSpacing
    return (FunDef funName typeArgs exp)

typeOrFunDef :: Parser TypeOrFunDef
typeOrFunDef = do
    try typeDef <|> funDef

parser :: Parser Minerva
parser =
    typeOrFunDef `sepBy` skipSpacing