module Main (main) where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit (Test(..), Assertable(assert))
import Test.Framework.Providers.API (Test(Test))


testMap :: [(String, Test.HUnit.Test)] -> [Test.HUnit.Test]
testMap = map (uncurry TestLabel)


tests :: [Test.Framework.Providers.API.Test]
tests = hUnitTestToTests $ TestList $ testMap labeledTests

main :: IO ()
main = defaultMain tests


labeledTests :: [(String, Test.HUnit.Test)]
labeledTests = [("First one", TestCase $ assert (7 == 5)),
                ("Second one", TestCase $ assert (7 == 7))]