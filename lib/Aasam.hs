module Aasam where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import qualified Data.List.NonEmpty
import Grammars
import Util

infixl 5 =.=
(=.=) :: PrecedenceProduction -> PrecedenceProduction -> Bool
(=.=) (Infixl _ _) (Infixl _ _) = True
(=.=) (Infixr _ _) (Infixr _ _) = True
(=.=) (Prefix _ _) (Prefix _ _) = True
(=.=) (Postfix _ _) (Postfix _ _) = True
(=.=) (Closed _) (Closed _) = True
(=.=) _ _ = False

m :: Precedence -> ContextFree
m prec =
    (NonTerminal "", Set.empty)
    where

        (prefixes, postfixes) = (select idPrefix, select idPostfix) where
            idPrefix ((Prefix _ _) : _) = True
            idPrefix _ = False
            idPostfix ((Postfix _ _) : _) = True
            idPostfix _ = False
            select f = unwrap $ find f classes where
                unwrap (Just x) = x
                unwrap Nothing = []
        


        classes = Set.toList prec |> groupBy (=.=)


        -- assignPQ :: [[PrecedenceProduction]] -> [[(Int, Int, PrecedenceProduction)]]
