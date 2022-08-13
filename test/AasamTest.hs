module Main (main) where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit (Test(..), Assertable(assert), Assertion, assertEqual)
import Test.Framework.Providers.API (Test(Test))
import qualified Data.Set as Set
import Aasam
import qualified Data.List as List
import Data.List.NonEmpty ( fromList )


testMap :: Eq a => Show a => [(String, a, a)] -> [Test.HUnit.Test]
testMap = List.map (\(label, x, y) -> TestLabel label (TestCase (assertEqual "" x y)))


tests :: [Test.Framework.Providers.API.Test]
tests = hUnitTestToTests $ TestList labeledTests

main :: IO ()
main = defaultMain tests

empt :: [a]
empt = []


labeledTests :: [Test.HUnit.Test]
labeledTests = testMap [("okay", Right Positivity, m pg)]
-- labeledTests = testMap [("okay", 20, m pg |> unwrapOr (nt 0 0 0, Set.empty) |> snd |> List.length)]


pg :: Precedence
pg = Set.fromList [
          Postfix 4 (fromList ["?"])
        , Infixl 3 (fromList ["+"])
        , Infixl 1 (fromList ["*"])
        , Postfix 2 (fromList ["!"])
        , Closed (fromList ["int"])
        ]


d :: Maybe ContextFree -> ContextFree
d (Just x) = x
d Nothing = (NonTerminal "String", Set.empty)