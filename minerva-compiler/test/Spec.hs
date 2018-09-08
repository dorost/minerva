import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Monad

import Text.Megaparsec
import Data.Text.Arbitrary

import Data.Either
import Data.Text.IO

import Parser
import Type

main :: IO ()
main = hspec $ parallel $ do
    describe "parser" $ do
        it "Does not crash on parsing input" $
            property $ \x -> let ast = runParser parser "" x in isLeft ast || isRight ast
    
    describe "Examples" $ do
        forM_ ["examples/bool.mrv", "examples/id.mrv", "examples/hole.mrv", "examples/nat.mrv", "examples/unit.mrv"] $ \f -> do
            it ("Parses and type checks succesfully: " <> f) $ do
                text <- Data.Text.IO.readFile f
                let parseResult = runParser parser "" text
                
                r <-
                    case parseResult of
                        Left err ->
                            return False
                        Right ast -> do
                            let typeDefs' = getTypeDefs' ast
                            let typeChecks = checkTypes' typeDefs' ast
                            return $ typeChecks == Nothing
                r `shouldBe` True
    



