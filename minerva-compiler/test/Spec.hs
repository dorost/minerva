import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Parser
import Text.Megaparsec
import Data.Text.Arbitrary

import Data.Either

main :: IO ()
main = hspec $ do
    describe "parser" $ do
        it "Does not crash on parsing input" $
            property $ \x -> let y = runParser parser "" x in isLeft y || isRight y
