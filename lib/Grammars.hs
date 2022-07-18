module Grammars where

import Data.Set (Set)
import qualified Data.Set as Set

newtype NonTerminal = NonTerminal String
newtype Terminal    = Terminal String
type CfgString      = [Either Terminal NonTerminal]
type CfgProduction  = (NonTerminal, CfgString)
type ContextFree    = (NonTerminal, Set CfgProduction)

newtype Infixl  = Infixl Closed deriving (Eq, Ord, Show)
newtype Infixr  = Infixr Closed deriving (Eq, Ord, Show)
newtype Prefix  = Prefix Closed deriving (Eq, Ord, Show)
newtype Postfix = Postfix Closed deriving (Eq, Ord, Show)
data Closed =
      Op Ae
    | SubInl Ae Infixl Ae
    | SubInr Ae Infixr Ae
    | SubPre Ae Prefix Ae
    | SubPost Ae Postfix Ae
    | SubClosed Ae Closed Ae
    deriving (Eq, Ord, Show)
type Ae = String

data PrecedenceProduction =
      Inl Int Infixl
    | Inr Int Infixr
    | Pre Int Prefix
    | Post Int Postfix
    | Closed Closed
    deriving (Eq, Ord, Show)

type Precedence = Set PrecedenceProduction
