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
    char ':'
    skipSpacing
    ts <- typeArgs
    skipSpacing
    return (TypeConstructor instanceName ts)

expr :: Maybe Expr -> Parser Expr
expr Nothing = do
    ref <- takeWhile1P Nothing (\c -> c /= ' ' && c /= '}' )
    skipSpacing
    let e = Var ref
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

typeArgs :: Parser Type
typeArgs = do
    skipSpacing
    t <- try (char '(' *> typeArgs <* char ')' )
         <|> Basic <$> funType
    skipSpacing
    To t <$> try (string "->" *> typeArgs)
         <|> return t
    
funDef :: Parser Expr
funDef = do
    funName <- noSpacing
    skipSpacing1
    char ':'
    skipSpacing
    targs <- typeArgs
    skipSpacing
    return (FunDef funName targs)

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
    skipSpacing
    return (FunDecl funName args e)

topLevel :: Parser Expr
topLevel =
    try typeDef <|> try funDef <|> funDecl

parser :: Parser Minerva
parser =
    topLevel `sepBy` skipSpacing