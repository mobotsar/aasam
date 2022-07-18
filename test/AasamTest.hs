module Main (main) where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit (Test(..), Assertable(assert), Assertion)
import Test.Framework.Providers.API (Test(Test))


testMap :: [(String, Bool)] -> [Test.HUnit.Test]
testMap = map (\(x, y) -> TestLabel x ((TestCase . assert) y))


tests :: [Test.Framework.Providers.API.Test]
tests = hUnitTestToTests $ TestList $ testMap labeledTests

main :: IO ()
main = defaultMain tests


labeledTests :: [(String, Bool)]
labeledTests = [("First one", 7 == 5),
                ("Second one", 7 == 7)]