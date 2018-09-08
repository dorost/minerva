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
    let fileName = "./examples/unit.mrv"
    f <- Data.Text.IO.readFile fileName
    
    let parseResult = runParser parser fileName f

    case parseResult of
        Left err ->
            putStrLn ("Error: " <> pack (show err))
        Right ast -> do
            print ast
            let typeDefs' = getTypeDefs' ast
            print typeDefs'
            let typeChecks = checkTypes' typeDefs' ast
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
                        let x = eval e prog
                        let t = checkType' typeDefs' e

                        case (t, x) of 
                            (Just (Right t1, s1), Right (e2,_)) -> putStrLn (prettyPrintExpr e2 <> " : " <> prettyPrintType t1)
                            (Just (Left err, _), _) -> putStrLn err
                            (_, Left err) -> putStrLn err
                return ()

