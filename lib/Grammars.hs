module Grammars where

import Data.Set (Set)
import qualified Data.Set as Set

newtype NonTerminal = NonTerminal String
newtype Terminal    = Terminal String
type CfgString      = [Either Terminal NonTerminal]
type CfgProduction  = (NonTerminal, CfgString)
type ContextFree    = (NonTerminal, Set CfgProduction)

newtype Infix  = Infix Closed deriving (Eq, Ord, Show)
newtype Prefix  = Prefix Closed deriving (Eq, Ord, Show)
newtype Postfix = Postfix Closed deriving (Eq, Ord, Show)
data Closed =
      Op Ae
    | ClosedInfix Ae Infix Ae
    | ClosedPrefix Ae Prefix Ae
    | ClosedPostfix Ae Postfix Ae
    | ClosedOuter Ae Closed Ae
    | ClosedInner Ae Ae
    deriving (Eq, Ord, Show)
type Ae = String

data PrecedenceProduction =
      Inl Int Infix
    | Inr Int Infix
    | Pre Int Prefix
    | Post Int Postfix
    | Closed Closed
    deriving (Ord, Show)

instance Eq PrecedenceProduction where
  (==) (Inl _ _) (Inl _ _) = True
  (==) (Inr _ _) (Inr _ _) = True
  (==) (Pre _ _) (Pre _ _) = True
  (==) (Post _ _) (Post _ _) = True
  (==) (Closed _) (Closed _) = True
  (==) _ _ = False

type Precedence = Set PrecedenceProduction
