{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.Text.IO
import Parser
import Text.Megaparsec
import Type
import Control.Monad
import Eval

main :: IO ()
main = do
    let fileName = "./examples/bool.mrv"
    f <- Data.Text.IO.readFile fileName
    
    let parseResult = runParser parser fileName f

    case parseResult of
        Left err ->
            Prelude.putStrLn ("Error: " <> show err)
        Right ast -> do
            let typeCons = getTypeDefs ast
            let prog = loadProgram ast
            print ("minerva loaded")
            forever $ do
                inExpr <- Data.Text.IO.getLine
                let parseResult = runParser expr "eval" inExpr
                case parseResult of
                    Left err ->
                        Prelude.putStrLn ("Error: " <> show err)
                    Right expr -> do    
                        print (eval expr prog)
                return ()

