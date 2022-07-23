{-# LANGUAGE DeriveDataTypeable #-}

module Grammars where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.NonEmpty ( NonEmpty )
import Data.Data ( Data, Typeable )

newtype NonTerminal = NonTerminal String deriving (Eq, Ord)
newtype Terminal    = Terminal String deriving (Eq, Ord)
type CfgString      = [Either Terminal NonTerminal]
type CfgProduction  = (NonTerminal, CfgString)
type ContextFree    = (NonTerminal, Set CfgProduction)

data PrecedenceProduction =
      Prefix  Int (NonEmpty String)
    | Postfix Int (NonEmpty String)
    | Infixl  Int (NonEmpty String)
    | Infixr  Int (NonEmpty String)
    | Closed      (NonEmpty String)
    deriving (Eq, Ord, Show, Typeable, Data)
type Precedence = Set.Set PrecedenceProduction

-- Precedences must always be positive integers.
-- 