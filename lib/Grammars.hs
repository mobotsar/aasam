{-# LANGUAGE DeriveDataTypeable #-}

module Grammars where

import Data.Data (Data, Typeable)
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import qualified Data.Set as Set

newtype NonTerminal =
    NonTerminal String
    deriving (Eq, Ord, Show)

newtype Terminal =
    Terminal String
    deriving (Eq, Ord, Show)

type CfgString = [Either Terminal NonTerminal]

-- |The type of a context-free production. The left and right items correspond respectively to the left and right hand sides of a production rule.
type CfgProduction = (NonTerminal, CfgString)

-- |The type of a context-free gramar. On the left the starting non-terminal, and on the right is the set of productions in the grammar.
type ContextFree = (NonTerminal, Set CfgProduction)

-- |The type of a distfix precedence production.
--
-- Int parameters are precedences.
--
-- NonEmpty String parameters are lists of non-terminal symbols expressed as strings.
--  The particular data constructor used implies the interspersal pattern of non-terminals in the terminal list when the production is interpreted.
--  For example,
--
-- > Infixl 1 (fromList ["?", ":"])
-- corresponds to the left-associative production, E -> E ? E : E.
data PrecedenceProduction
    = Prefix Int (NonEmpty String)
    | Postfix Int (NonEmpty String)
    | Infixl Int (NonEmpty String)
    | Infixr Int (NonEmpty String)
    | Closed (NonEmpty String)
    deriving (Eq, Ord, Show, Typeable, Data)

-- |The type of a distfix precedence grammar. The following must be true of any parameter to `Aasam.m`.
--
--      * All precedences must be positive integers.
--      * No initial word may also be a subsequent word of another production.
--      * No initial sequence of words may also be the whole sequence of another production.
--      * No precedence of a production of one fixity may also be the precedence of a production of another fixity.
--      * The set of precedences must be either empty or the set of integers between 1 and greatest precedence, inclusive.
type Precedence = Set PrecedenceProduction
