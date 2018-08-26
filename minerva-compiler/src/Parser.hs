{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Text
import Text.Megaparsec
import Text.Megaparsec.Char


data TypeOrFunDef
    = TypeDef Text [TypeConstructor]
    deriving Show

type Minerva = [TypeOrFunDef]

data TypeConstructor
    = TypeConstructor Text
    deriving Show

type Parser = Parsec () Text

skipSpacing :: Parser Text
skipSpacing =
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


typeOrFunDef :: Parser TypeOrFunDef
typeOrFunDef = do
    typeName <- noSpacing
    skipSpacing
    string "=:"
    skipSpacing
    char '{'
    typeConstructors <- typeConstructor `sepBy` char ','
    char '}'
    return (TypeDef typeName typeConstructors)

parser :: Parser Minerva
parser =
    typeOrFunDef `sepBy` skipSpacing