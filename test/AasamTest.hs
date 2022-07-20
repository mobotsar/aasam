module Main (main) where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit (Test(..), Assertable(assert), Assertion, assertEqual)
import Test.Framework.Providers.API (Test(Test))
import qualified Data.Set as Set
import Grammars
import Aasam


testMap :: Eq a => Show a => [(String, a, a)] -> [Test.HUnit.Test]
testMap = map (\(label, x, y) -> TestLabel label (TestCase (assertEqual "" x y)))


tests :: [Test.Framework.Providers.API.Test]
tests = hUnitTestToTests $ TestList labeledTests

main :: IO ()
main = defaultMain tests

empt :: [a]
empt = []


labeledTests :: [Test.HUnit.Test]
labeledTests = empt
    -- ++ testMap [("DISPLAY exposed grammar format", precGrm, Set.fromList [Inl 2 (Infix (Op "+")),Inl 3 (Infix (Op "*")),Post 1 (Postfix (Op "?")),Closed (Op "x"),Closed (SubInfix "if" (Infix (SubClosedInner "then" "else")) "fi")])]
    -- ++ testMap [("DISPLAY conversion to internal format", flatPrecGrm, Set.fromList [FlatPostfix 1 ["?"],FlatInfixl 2 ["+"],FlatInfixl 3 ["*"],FlatClosed ["if","then","else","fi"],FlatClosed ["x"]])]


precGrm :: Precedence 
precGrm = Set.fromList [Inl 3 (Infix (Op "*")),
                        Inl 2 (Infix (Op "+")),
                        Post 1 (Postfix (Op "?")),
                        Closed (ClosedInfix "if" (Infix (ClosedInner "then" "else")) "fi"),
                        Closed (Op "x")]

flatPrecGrm :: FlatPrecedence 
flatPrecGrm = squish precGrm
