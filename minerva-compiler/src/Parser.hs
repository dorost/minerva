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

matchExpr :: Parser Expr
matchExpr = do
    string "case"
    skipSpacing
    e1 <- expr Nothing
    skipSpacing
    char '{'
    patterns <- sepBy patternExpr "|"
    skipSpacing
    char '}'
    skipSpacing
    return (Match e1 patterns)


patternExpr :: Parser (Pattern, Expr)
patternExpr = do
    p <- pattern
    string "=>"
    skipSpacing
    e <- expr Nothing
    skipSpacing
    return (p, e)

pattern ::  Parser Pattern
pattern = do
    skipSpacing
    instanceName <- noSpacing
    skipSpacing
    args <- many arg
    skipSpacing
    return (Pattern instanceName args)

expr :: Maybe Expr -> Parser Expr
expr Nothing = do
    ref <- takeWhile1P Nothing (\c -> c /= ' ' && c /= '}' && c /= '|' && c /= '=' && c /= '{' && c/= '\n')
    skipSpacing
    let e = Var ref
    try (expr (Just e)) <|> return e
expr (Just e) = do
    skipSpacing
    ref <- takeWhile1P Nothing (\c -> c /= ' ' && c /= '}' && c /= '|' && c /= '=' && c /= '{' && c/= '\n')
    let app = App e (Var ref)

    try (expr (Just app)) <|> return app


typeDef :: Parser Expr
typeDef = do
    typeName <- noSpacing
    skipSpacing1
    char '{'
    typeConstructors <- typeConstructor `sepBy` char '|'
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

arg :: Parser Text
arg = do
    argName <- takeWhile1P Nothing (\c -> c /= ' ' && c /= '\n' && c /= '{' && c /= '=')
    skipSpacing
    return argName
    
funDecl :: Parser Expr
funDecl = do
    funName <- noSpacing
    skipSpacing
    args <- many arg
    char '{'
    skipSpacing
    e <- matchExpr <|> expr Nothing
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