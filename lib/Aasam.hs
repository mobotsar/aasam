module Aasam where

import Grammars

import Data.Set
import qualified Data.Set as Set
import Data.List
import Util

data FlatPP =
      FlatPrefix  Int [Ae]
    | FlatPostfix Int [Ae]
    | FlatInfixl  Int [Ae]
    | FlatInfixr  Int [Ae]
    | FlatClosed      [Ae]
    deriving (Eq, Ord, Show)
type FlatPrecedence = Set.Set FlatPP

renderFlat :: PrecedenceProduction -> FlatPP
renderFlat (Inl n sub)  = FlatInfixl n (flattenInfix sub)
renderFlat (Inr n sub)  = FlatInfixr n (flattenInfix sub)
renderFlat (Pre n sub)  = FlatPrefix n (flattenPrefix sub)
renderFlat (Post n sub) = FlatPostfix n (flattenPostfix sub)
renderFlat (Closed sub) = FlatClosed (flattenClosed sub)

flattenInfix :: Infix -> [Ae]
flattenInfix (Infix sub) = flattenClosed sub

flattenPrefix :: Prefix -> [Ae]
flattenPrefix (Prefix sub) = flattenClosed sub

flattenPostfix :: Postfix -> [Ae]
flattenPostfix (Postfix sub) = flattenClosed sub

flattenClosed :: Closed -> [Ae]
flattenClosed (Op sub) = [sub]
flattenClosed (ClosedInfix ae0 inr ae1) = ae0 : flattenInfix inr ++ [ae1]
flattenClosed (ClosedPrefix ae0 pre ae1) = ae0 : flattenPrefix pre ++ [ae1]
flattenClosed (ClosedPostfix ae0 post ae1) = ae0 : flattenPostfix post ++ [ae1]
flattenClosed (ClosedOuter ae0 closed ae1) = ae0 : flattenClosed closed ++ [ae1]
flattenClosed (ClosedInner ae0 ae1) = ae0 : [ae1]

squish :: Precedence -> FlatPrecedence
squish = Set.map renderFlat

divide :: Precedence -> [[FlatPP]]
divide = squish >. Set.toList >. group


m :: Precedence -> ContextFree
m prec =
    let classes = divide prec in
    let prefixes = find (== (Pre)) classes in
    let postfixes = find (== (Post)) classes in
    (NonTerminal "", Set.empty)


