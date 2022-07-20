module Grammars where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.NonEmpty

newtype NonTerminal = NonTerminal String
newtype Terminal    = Terminal String
type CfgString      = [Either Terminal NonTerminal]
type CfgProduction  = (NonTerminal, CfgString)
type ContextFree    = (NonTerminal, Set CfgProduction)

data PrecedenceProduction =
      Prefix  Int (NonEmpty String)
    | Postfix Int (NonEmpty String)
    | Infixl  Int (NonEmpty String)
    | Infixr  Int (NonEmpty String)
    | Closed      (NonEmpty String)
    deriving (Eq, Ord, Show)
type Precedence = Set.Set PrecedenceProduction
