module Distfixm (someFunc) where

import Grammars
import Data.Set

someFunc :: IO ()
someFunc = putStrLn "someFunc"

grm :: Distfix
grm = singleton (In (Infix (Op "")))