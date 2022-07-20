{-# LANGUAGE RankNTypes #-}
module Aasam where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import qualified Data.List.NonEmpty
import Grammars
import Util
import Data.List.NonEmpty (NonEmpty)


infixl 5 =.=
(=.=) :: PrecedenceProduction -> PrecedenceProduction -> Bool
(=.=) (Infixl _ _) (Infixl _ _) = True
(=.=) (Infixr _ _) (Infixr _ _) = True
(=.=) (Prefix _ _) (Prefix _ _) = True
(=.=) (Postfix _ _) (Postfix _ _) = True
(=.=) (Closed _) (Closed _) = True
(=.=) _ _ = False


m :: Precedence -> ContextFree
m prec = (NonTerminal "", Set.empty) where
    classes = Set.toList prec |> groupBy (=.=)

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
            qsets p = foldl (\a q -> Set.union a $ rule prec p q words) Set.empty [0..qbound] where
                words = doGeneric prod (\x y -> Data.List.NonEmpty.toList y)

    nt prec p q = NonTerminal (show prec ++ show p ++ show q)

    prefixCfg :: Set CfgProduction
    prefixCfg = convertClass rule prefixes where
        rule prec p q words = Set.singleton a where
            a = (nt prec p q, [Right (nt (prec - 1) (p + 1) q)])

    postfixCfg :: Set CfgProduction
    postfixCfg = convertClass rule postfixes where
        rule prec p q words = Set.singleton a where
            a = (nt prec p q, [Right (nt (prec - 1) p (q + 1))])

    infixlCfg :: Set CfgProduction
    infixlCfg = convertClass rule infixls where
        rule prec p q words = Set.fromList [a, b] where
            a = (nt prec p q, Right (nt prec 0 q) : inner ++ [Right (nt (prec - 1) p 0)]) where
                inner = map (Left . Terminal) words |> distribute (Right (NonTerminal "Start")) -- change this to change extendedness
            b = (nt prec p q, [Right (nt (prec - 1) p q)])
    
    infixrCfg :: Set CfgProduction
    infixrCfg = convertClass rule infixrs where
        rule prec p q words = Set.fromList [a, b] where
            a = (nt prec p q, Right (nt (prec - 1) 0 q) : inner ++ [Right (nt prec p 0)]) where
                inner = map (Left . Terminal) words |> distribute (Right (NonTerminal "Start")) -- change this to change extendedness
            b = (nt prec p q, [Right (nt (prec - 1) p q)])

    -- closedCfg :: Set CfgProduction
    -- closedCfg = convertClass rule closes where
    --     rule prec p q words = Set.fromList [a, b, c] where
    --         a = 
    --         b =
    --         c =

    distribute :: a -> [a] -> [a]
    distribute _ [] = []
    distribute _ [x] = [x]
    distribute e (x : xs) = x : e : distribute e xs




                    -- rule q = Set.union a b
                        -- a = Set.singleton (nt prec p q, Right (nt prec 0 q) : inner ++ [Right (nt (prec - 1) p 0)]) where
                        --     inner = []
                        -- b = Set.singleton (nt prec p q, []) 


        -- move :: Set CfgProduction
        -- move = foldl (\a e -> Set.union a (transform_class e)) Set.empty distfixes where
        --     transform_class :: [(Int, Int, PrecedenceProduction)] -> Set CfgProduction
        --     transform x = 




doGeneric :: PrecedenceProduction -> (Int -> NonEmpty String -> a) -> a
doGeneric (Prefix prec words) f = f prec words
doGeneric (Postfix prec words) f = f prec words
doGeneric (Infixl prec words) f = f prec words
doGeneric (Infixr prec words) f = f prec words
doGeneric (Closed words) f = f 0 words


