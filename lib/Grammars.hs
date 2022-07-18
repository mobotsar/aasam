{-# OPTIONS_GHC -Wno-missing-methods #-}

module Grammars where

import Data.Set (Set)
import qualified Data.Set as Set

newtype Infix = Infix Closed
newtype Prefix = Prefix Closed
newtype Postfix = Postfix Closed
data Closed = Op Op | SubIn Infix | SubPre Prefix | SubPost Postfix | SubClosed Closed
type Op = String

data DistfixRhs = In Infix | Pre Prefix | Post Postfix | Closed Closed | Ae Op

instance Eq DistfixRhs
instance Ord DistfixRhs 

type Distfix = Set DistfixRhs