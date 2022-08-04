module Aasam where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import qualified Data.List.NonEmpty as DLNe
import Grammars
import Util
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Function (on)

import Data.Data (toConstr)
import qualified Data.List as List


-- TODO: Make multiple operators of identical precedence and fixity work.
-- TODO: Add constraints such that all invalid input grammars are rejected.

doGeneric :: PrecedenceProduction -> (Int -> NonEmpty String -> a) -> a
doGeneric (Prefix prec words) f = f prec words
doGeneric (Postfix prec words) f = f prec words
doGeneric (Infixl prec words) f = f prec words
doGeneric (Infixr prec words) f = f prec words
doGeneric (Closed words) f = f 0 words

getWords :: PrecedenceProduction -> [String]
getWords = flip doGeneric (\_ y -> DLNe.toList y)

prec :: PrecedenceProduction -> Int
prec = flip doGeneric const

nt :: Int -> Int -> Int -> NonTerminal
nt prec p q = NonTerminal (show prec ++ show p ++ show q)

-- TODO: write a proper implementation of this that doesn't depend on List
groupSetBy :: Ord a => (a -> a -> Bool) -> Set a -> Set (Set a)
groupSetBy projection = Set.toList >. groupBy projection >. map Set.fromList >. Set.fromList

makeClasses :: Precedence -> Set Precedence
makeClasses = groupSetBy fixeq where
        fixeq = on (==) toConstr
        -- equivalence relation of fixity on precedence productions

type UniquenessPair = (PrecedenceProduction, Precedence)

-- This function returns a sae of upairs. A upair contains a production of a single precedence on the left,
-- and the set of all productions of that precedence on the right (including the one on the left).
classToPairSet :: Precedence -> Set UniquenessPair
classToPairSet = groupSetBy preceq >. Set.map pair where
    pair :: Precedence -> UniquenessPair
    pair prec = (Set.elemAt 0 prec, prec)
    preceq :: PrecedenceProduction -> PrecedenceProduction -> Bool
    preceq a b = prec a == prec b

pairifyClasses :: Set Precedence -> Set (Set UniquenessPair)
pairifyClasses = Set.map classToPairSet

type PqQuad = (Int, Int, PrecedenceProduction, Precedence)

pqboundProduction :: Set UniquenessPair -> Set UniquenessPair -> UniquenessPair -> PqQuad
pqboundProduction pre post (r, s) = (greater pre $ prec r, greater post $ prec r, r, s) where
    greater :: Set UniquenessPair -> Int -> Int
    greater upairs n = Set.size $ Set.filter (\(r, _) -> prec r > n) upairs

pqboundClasses :: Set UniquenessPair -> Set UniquenessPair -> Set (Set UniquenessPair) -> Set (Set PqQuad)
pqboundClasses pre post = Set.map (Set.map (pqboundProduction pre post))

intersperseStart :: NonEmpty String -> CfgString
intersperseStart = DLNe.map (Left . Terminal) >. DLNe.intersperse (Right (NonTerminal "!start")) >. DLNe.toList


fill :: Precedence -> Set CfgProduction -> Set CfgProduction
fill s cfgprods = Set.union withTerminals withoutTerminals where
    (left, withoutTerminals) = Set.partition hasTerminal cfgprods where
        hasTerminal :: CfgProduction -> Bool
        hasTerminal (_, words) = List.any isTerminal words
    isTerminal :: Either Terminal NonTerminal -> Bool
    isTerminal (Right (NonTerminal _)) = False
    isTerminal (Left (Terminal _)) =  True
    withTerminals = fill' s left where
        -- TODO: write a proper implementation of this composition that doesn't depend on List
        fill' s = Set.toList >. repeat >. zipWith reset (Set.toList s) >. concat >. Set.fromList where
            reset :: PrecedenceProduction -> [CfgProduction] -> [CfgProduction]
            reset pp = map fn where
                fn :: CfgProduction -> CfgProduction
                fn (x, rhs) = (x, re rhs) where
                    re :: CfgString -> CfgString
                    re str =
                        case pp of
                            Infixl prec words -> okie str words
                            Infixr prec words -> okie str words
                            Closed words -> clokie str words
                            _ -> error "This should be impossible. Somehow, I got a CfgPorduction with no terminals."
                        where
                            okie :: CfgString -> NonEmpty String -> CfgString
                            okie str words = List.head str : intersperseStart words ++ [List.last str]
                            clokie :: CfgString -> NonEmpty String -> CfgString
                            clokie str words =
                                if isTerminal $ head str then
                                    intersperseStart words ++ [last str]
                                else
                                    head str : intersperseStart words

-- AE production on `closedrule` must go to a non-terminal
-- rules: 
prerule :: Int -> Int -> PqQuad -> Set CfgProduction
prerule p q (_, _, r, s) = fill s $ Set.singleton (nt (prec r) p q, [Right (nt (prec r - 1) (p + 1) q)])

postrule :: Int -> Int -> PqQuad -> Set CfgProduction
postrule p q (_, _, r, s) = fill s $ Set.singleton (nt (prec r) p q, [Right (nt (prec r - 1) p (q + 1))])