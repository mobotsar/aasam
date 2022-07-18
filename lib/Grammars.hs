module Grammars where

import Data.Set (Set)
import qualified Data.Set as Set

newtype NonTerminal = NonTerminal String
newtype Terminal    = Terminal String
type CfgString      = [Either Terminal NonTerminal]
type CfgProduction  = (NonTerminal, CfgString)
type ContextFree    = (NonTerminal, Set CfgProduction)

newtype Infixl  = Infixl Closed deriving (Eq, Ord)
newtype Infixr  = Infixr Closed deriving (Eq, Ord)
newtype Prefix  = Prefix Closed deriving (Eq, Ord)
newtype Postfix = Postfix Closed deriving (Eq, Ord)
data Closed     = Op Ae | SubInl Infixl | SubInr Infixr | SubPre Prefix | SubPost Postfix | SubClosed Closed deriving (Eq, Ord)
type Ae         = String

data PrecedenceProduction = Inl (Infixl, Int) | Inr (Infixr, Int) | Pre (Prefix, Int) | Post (Postfix, Int) | Closed Closed deriving (Eq, Ord)

type Precedence = Set PrecedenceProduction
