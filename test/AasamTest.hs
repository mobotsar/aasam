module Main (main) where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit (Test(..), Assertable(assert), Assertion, assertEqual)
import Test.Framework.Providers.API (Test(Test))


testMap :: Eq a => Show a => [(String, a, a)] -> [Test.HUnit.Test]
testMap = map (\(label, x, y) -> TestLabel label (TestCase (assertEqual "" x y)))


tests :: [Test.Framework.Providers.API.Test]
tests = hUnitTestToTests $ TestList labeledTests

main :: IO ()
main = defaultMain tests


labeledTests :: [Test.HUnit.Test]
labeledTests =
    testMap [("First one", 6, 6), ("Second one", 7, 8)] ++
    testMap [("Third", [], [34])]