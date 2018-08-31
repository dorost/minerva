{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.Text
import Data.Text.IO
import Parser
import Text.Megaparsec
import Type
import Control.Monad
import Eval
import AST


main :: IO ()
main = do
    let fileName = "./examples/bool.mrv"
    f <- Data.Text.IO.readFile fileName
    
    let parseResult = runParser parser fileName f

    case parseResult of
        Left err ->
            Prelude.putStrLn ("Error: " <> show err)
        Right ast -> do
            print ast
            let typeDefs = getTypeDefs ast
            print typeDefs
            let typeChecks = checkTypes typeDefs ast
            print typeChecks
            let prog = loadProgram ast
            print ("minerva loaded")
            forever $ do
                inExpr <- Data.Text.IO.getLine
                let parseResult = runParser (expr Nothing) "eval" inExpr
                case parseResult of
                    Left err ->
                        Prelude.putStrLn ("Error: " <> show err)
                    Right e -> do
                        print e
                        print ()
                        Data.Text.IO.putStrLn (prettyPrintExpr (eval e prog) <> " : " <> prettyPrintType (checkType e typeDefs))

                return ()

