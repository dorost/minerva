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
            print ast
            let typeCons = getTypeDefs ast
            print (typeCons)
            print (checkTypes typeCons ast)
            let prog = map getToplevel ast
            print (prog)
            --forEver $
            --    inExpr <- Data.Text.IO.getLine

