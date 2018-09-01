{-# LANGUAGE OverloadedStrings #-}

module Main where
import Prelude hiding (putStrLn)
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
    let fileName = "./examples/shouldfail.mrv"
    f <- Data.Text.IO.readFile fileName
    
    let parseResult = runParser parser fileName f

    case parseResult of
        Left err ->
            putStrLn ("Error: " <> pack (show err))
        Right ast -> do
            print ast
            let typeDefs = getTypeDefs ast
            print typeDefs
            let typeChecks = checkTypes typeDefs ast
            print typeChecks
            let prog = loadProgram ast
            putStrLn "minerva loaded"
            forever $ do
                inExpr <- Data.Text.IO.getLine
                let parseResult = runParser (expr Nothing) "eval" inExpr
                case parseResult of
                    Left err ->
                        putStrLn ("Error: " <> pack (show err))
                    Right e -> do
                        print e
                        print ()
                        let x = eval e prog

                        case x of 
                            Right e2 -> putStrLn (prettyPrintExpr e2 <> " : " <> prettyPrintType (checkType e typeDefs))
                            Left err -> putStrLn err
                return ()

