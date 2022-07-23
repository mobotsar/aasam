module Main (main) where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit (Test(..), Assertable(assert), Assertion, assertEqual)
import Test.Framework.Providers.API (Test(Test))
import qualified Data.Set as Set
import Grammars
import Aasam
import qualified Data.List as List
-- import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.NonEmpty


testMap :: Eq a => Show a => [(String, a, a)] -> [Test.HUnit.Test]
testMap = List.map (\(label, x, y) -> TestLabel label (TestCase (assertEqual "" x y)))


tests :: [Test.Framework.Providers.API.Test]
tests = hUnitTestToTests $ TestList labeledTests

main :: IO ()
main = defaultMain tests

empt :: [a]
empt = []


labeledTests :: [Test.HUnit.Test]
labeledTests = testMap [("hello", d (m pg), (NonTerminal "String", Set.empty))]

pg :: Precedence 
pg = Set.fromList [
          Postfix 4 (singleton "?")
        , Infixl 3 (singleton ":" <> singleton "=")
        , Infixl 1 (singleton "*")
        , Postfix 2 (singleton "!")
        , Closed (singleton "atom")
        ]


d :: Maybe ContextFree -> ContextFree
d (Just x) = x
d Nothing = (NonTerminal "String", Set.empty)

