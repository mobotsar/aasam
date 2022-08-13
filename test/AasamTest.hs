module Main (main) where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit (Test(..), Assertable(assert), Assertion, assertEqual)
import Test.Framework.Providers.API (Test(Test))
import qualified Data.Set as Set
import Aasam
import qualified Data.List as List
import Data.List.NonEmpty ( fromList, xor )


testMap :: (Eq a, Show a) => [(String, a, a)] -> [Test.HUnit.Test]
testMap = List.map (\(label, x, y) -> TestLabel label (TestCase (assertEqual "" x y)))


tests :: [Test.Framework.Providers.API.Test]
tests = hUnitTestToTests $ TestList labeledTests

main :: IO ()
main = defaultMain tests

empt :: [a]
empt = []


labeledTests :: [Test.HUnit.Test]
labeledTests = testMap [("okay", Just 20, Just (Set.size (snd (un (m pg0)))))]
-- labeledTests = testMap [("okay", Nothing, Just (m pg0))]


pg0 :: Precedence
pg0 = Set.fromList [
          Postfix 4 (fromList ["?"])
        , Infixl 3 (fromList ["+"])
        , Infixl 1 (fromList ["*"])
        , Postfix 2 (fromList ["!"])
        , Closed (fromList ["int"])
        ]

un :: Either a b -> a
un (Left x) = x
un _ = error "fail"

pg1 =
    Set.fromList [
          Closed (fromList ["x"])
        , Closed (fromList ["OPEN", "CLOSE"])
    ]


d :: Maybe ContextFree -> ContextFree
d (Just x) = x
d Nothing = (NonTerminal "String", Set.empty)