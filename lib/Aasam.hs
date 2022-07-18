module Aasam (m) where

import Grammars
    ( Ae,
      Closed(..),
      ContextFree,
      Infixl(..),
      Infixr(..),
      NonTerminal(NonTerminal),
      Postfix(..),
      Precedence,
      PrecedenceProduction(..),
      Prefix(..) )

import Data.Set
import qualified Data.Set as Set

data FlatPP =
      FlatPrefix  Int [Ae]
    | FlatPostfix Int [Ae]
    | FlatInfixl  Int [Ae]
    | FlatInfixr  Int [Ae]
    | FlatClosed      [Ae]
    deriving (Eq, Ord, Show)
type FlatPrecedence = Set.Set FlatPP

renderFlat :: PrecedenceProduction -> FlatPP
renderFlat (Inl n sub)  = FlatInfixl n (flattenInfixl sub)
renderFlat (Inr n sub)  = FlatInfixr n (flattenInfixr sub)
renderFlat (Pre n sub)  = FlatPrefix n (flattenPrefix sub)
renderFlat (Post n sub) = FlatPostfix n (flattenPostfix sub)
renderFlat (Closed sub) = FlatClosed (flattenClosed sub)

flattenInfixl :: Infixl -> [Ae]
flattenInfixl (Infixl sub) = flattenClosed sub

flattenInfixr :: Infixr -> [Ae]
flattenInfixr (Infixr sub) = flattenClosed sub

flattenPrefix :: Prefix -> [Ae]
flattenPrefix (Prefix sub) = flattenClosed sub

flattenPostfix :: Postfix -> [Ae]
flattenPostfix (Postfix sub) = flattenClosed sub

flattenClosed :: Closed -> [Ae]
flattenClosed (Op sub) = [sub]
flattenClosed (SubInl ae0 inl ae1) = ae0 : flattenInfixl inl ++ [ae1]
flattenClosed (SubInr ae0 inr ae1) = ae0 : flattenInfixr inr ++ [ae1]
flattenClosed (SubPre ae0 pre ae1) = ae0 : flattenPrefix pre ++ [ae1]
flattenClosed (SubPost ae0 post ae1) = ae0 : flattenPostfix post ++ [ae1]
flattenClosed (SubClosed ae0 closed ae1) = ae0 : flattenClosed closed ++ [ae1]

m :: Precedence -> ContextFree
m prec = (NonTerminal "", empty)

