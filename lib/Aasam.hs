-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
module Aasam where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import qualified Data.List.NonEmpty
import Grammars
import Util
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Function (on)

import Data.Data (toConstr)
import GHC.Stats (RTSStats(par_copied_bytes))
import qualified Data.Foldable


-- TODO: Make multiple operators of identical precedence and fixity work. A post-generation pass is probably easiest.
-- TODO: Add constraints such that invalid input grammars are rejected.
-- TODO: Add a production for AE -> (all the operators words).
-- TODO: Seems like everything except closedCfg is working properly. Make it all work properly.
m :: Precedence -> Maybe ContextFree
m prec = Just cfg where
    classes = Set.toList prec |> groupBy (on (==) toConstr)

    (prefixes', postfixes', infixls, infixrs, closes) =
        (select idPrefix, select idPostfix, selectPQ idInfixl, selectPQ idInfixr, selectPQ idClosed) where
            idPrefix ((Prefix _ _) : _) = True
            idPrefix _ = False
            idPostfix ((Postfix _ _) : _) = True
            idPostfix _ = False
            idInfixl ((Infixl _ _) : _) = True
            idInfixl _ = False
            idInfixr ((Infixr _ _) : _) = True
            idInfixr _ = False
            idClosed ((Closed _) : _) = True
            idClosed _ = False
            select f = unwrapOr [] $ find f classes
            selectPQ = map assignPQ . select

    assignPQ :: PrecedenceProduction -> (Int, Int, Int, PrecedenceProduction)
    assignPQ prod = doGeneric prod (\x _ -> (x, higher prefixes' x, higher postfixes' x, prod)) where
        higher prods bound = filter (\prod -> doGeneric prod const > bound) prods |> length

    prefixes = map assignPQ prefixes'
    postfixes = map assignPQ postfixes'

    -- rule :: prec -> p -> q -> words -> cfg-productions
    convertClass rule = foldl (\a e -> Set.union a $ psets e) Set.empty where
        psets (prec, pbound, qbound, prod) = foldl (\a p -> Set.union a $ qsets p) Set.empty [0..pbound] where
            qsets p = foldl (\a q -> Set.union a $ rule prec p q $ getWords prod) Set.empty [0..qbound]

    nt prec p q = NonTerminal (show prec ++ show p ++ show q)

    prefixCfg :: Set CfgProduction
    prefixCfg = Set.empty
    -- prefixCfg = convertClass rule prefixes where
    --     rule prec p q words = Set.singleton a where
    --         a = (nt prec p q, [Right (nt (prec - 1) (p + 1) q)])

    postfixCfg :: Set CfgProduction
    postfixCfg = Set.empty 
    -- postfixCfg = convertClass rule postfixes where
    --     rule prec p q words = Set.singleton a where
    --         a = (nt prec p q, [Right (nt (prec - 1) p (q + 1))])

    infixlCfg :: Set CfgProduction
    infixlCfg = Set.empty 
    -- infixlCfg = convertClass rule infixls where
    --     rule prec p q words = Set.fromList [a, b] where
    --         a = (nt prec p q, Right (nt prec 0 q) : inner ++ [Right (nt (prec - 1) p 0)]) where
    --             inner = map (Left . Terminal) words |> intersperse (Right (NonTerminal "!start")) -- change this to change extendedness
    --         b = (nt prec p q, [Right (nt (prec - 1) p q)])

    infixrCfg :: Set CfgProduction
    infixrCfg = Set.empty 
    -- infixrCfg = convertClass rule infixrs where
    --     rule prec p q words = Set.fromList [a, b] where
    --         a = (nt prec p q, Right (nt (prec - 1) 0 q) : inner ++ [Right (nt prec p 0)]) where
    --             inner = map (Left . Terminal) words |> intersperse (Right (NonTerminal "!start")) -- change this to change extendedness
    --         b = (nt prec p q, [Right (nt (prec - 1) p q)])

    closedCfg :: Set CfgProduction
    -- closedCfg = Set.empty 
    closedCfg = convertClass rule closes where
        rule prec p q words = foldl (\a e -> Set.union a $ jsets e) Set.empty (zip prefixes [1..p]) where
            jsets ((preprec, prep, preq, pre), i) = foldl (\a e -> Set.union a $ single e) Set.empty (zip postfixes [1..q]) where
                single ((postprec, postp, postq, post), j) = Set.fromList [a, b, c] where
                    a = (nt 0 p q, [Right (NonTerminal "AE")])
                    b = (nt 0 p q, op ++ [Right (nt preprec (p - i) 0)]) where
                        op = map (Left . Terminal) (getWords pre) |> intersperse (Right (NonTerminal "!start"))
                    c = (nt 0 p q, Right (nt postprec 0 (q - j)) : op) where
                        op = map (Left . Terminal) (getWords post) |> intersperse (Right (NonTerminal "!start"))

    cfg :: ContextFree
    cfg = (NonTerminal "!start", assignStartSym prods) where
        prods = foldl Set.union Set.empty [prefixCfg, postfixCfg, infixlCfg, infixrCfg, closedCfg]
        assignStartSym prods = Set.map (\(k, l) -> if k == maxp then (NonTerminal "!start", l) else (k, l)) prods where
            maxp = maximumBy (\(NonTerminal x, _) (NonTerminal y, _) -> compare x y) prods |> fst

    getWords :: PrecedenceProduction -> [String]
    getWords = flip doGeneric (\_ y -> Data.List.NonEmpty.toList y)



doGeneric :: PrecedenceProduction -> (Int -> NonEmpty String -> a) -> a
doGeneric (Prefix prec words) f = f prec words
doGeneric (Postfix prec words) f = f prec words
doGeneric (Infixl prec words) f = f prec words
doGeneric (Infixr prec words) f = f prec words
doGeneric (Closed words) f = f 0 words


