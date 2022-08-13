{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Evaluate" #-}
module Main
    ( main
    ) where

import Aasam
import qualified Data.List as List
import Data.List.NonEmpty (fromList, xor)
import qualified Data.Set as Set
import Test.Framework (defaultMain)
import Test.Framework.Providers.API (Test(Test))
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit (Assertable(assert), Assertion, Test(..), assertEqual)

testMap :: (Eq a, Show a) => [(String, a, a)] -> [Test.HUnit.Test]
testMap = List.map (\(label, x, y) -> TestLabel label (TestCase (assertEqual "" x y)))

tests :: [Test.Framework.Providers.API.Test]
tests = hUnitTestToTests $ TestList labeledTests

main :: IO ()
main = defaultMain tests

empt :: [a]
empt = []

labeledTests :: [Test.HUnit.Test]
labeledTests =
    []
    -- ++ testMap [("okay", Just 20, Just (Set.size (snd (un (m pg0)))))]
     ++
    testMap [("under", Nothing, Just (m pg1))]

pg0 :: Precedence
pg0 =
    Set.fromList
        [ Postfix 4 (fromList ["?"])
        , Infixl 4 (fromList ["+"])
        , Infixl 0 (fromList ["+"])
        , Postfix 2 (fromList ["!", "?"])
        , Closed (fromList ["int"])
        ]

un :: Either a b -> a
un (Left x) = x
un _ = error "fail"

pg1 :: Set.Set PrecedenceProduction
pg1 =
    Set.fromList
        [ Infixr 3 (fromList ["="])
        , Prefix 2 (fromList ["λ", "."])
        , Closed (fromList ["x"])
        , Closed (fromList ["(", "$", ")"])
        ]

d :: Maybe ContextFree -> ContextFree
d (Just x) = x
d Nothing = (NonTerminal "String", Set.empty)
