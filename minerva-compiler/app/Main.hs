{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.Text.IO
import Parser
import Text.Megaparsec

main :: IO ()
main = do
    let fileName = "./examples/bool.mrv"
    f <- Data.Text.IO.readFile fileName
    
    let parseResult = runParser parser fileName f

    case parseResult of
        Left err ->
            Prelude.putStrLn ("Error: " <> show err)
        Right ast ->
            print ast