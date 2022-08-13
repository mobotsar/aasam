module Aasam
    ( m
    , module Grammars
    , AasamError(..)
    ) where

import Data.Function (on)
import Data.List (groupBy)
import qualified Data.List.NonEmpty as DLNe
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Set (Set, insert, union)
import qualified Data.Set as Set
import Grammars
    ( CfgProduction
    , CfgString
    , ContextFree
    , NonTerminal(..)
    , Precedence
    , PrecedenceProduction(..)
    , Terminal(..)
    )
import Util ((>.), (|>), unwrapOr)

import Data.Bifunctor (Bifunctor(bimap))
import Data.Data (toConstr)
import qualified Data.Foldable
import qualified Data.List as List

doGeneric :: PrecedenceProduction -> (Int -> NonEmpty String -> a) -> a
doGeneric (Prefix prec words) f = f prec words
doGeneric (Postfix prec words) f = f prec words
doGeneric (Infixl prec words) f = f prec words
doGeneric (Infixr prec words) f = f prec words
doGeneric (Closed words) f = f 0 words

getWords :: PrecedenceProduction -> [String]
getWords = flip doGeneric (const DLNe.toList)

prec :: PrecedenceProduction -> Int
prec = flip doGeneric const

nt :: Int -> Int -> Int -> NonTerminal
nt prec p q = NonTerminal (show prec ++ show p ++ show q)

-- TODO: write a proper implementation of this that doesn't depend on List
groupSetBy :: Ord a => (a -> a -> Bool) -> Set a -> Set (Set a)
groupSetBy projection = Set.toList >. groupBy projection >. map Set.fromList >. Set.fromList

makeClasses :: Precedence -> Set Precedence
makeClasses = groupSetBy fixeq
  where
    fixeq = on (==) toConstr
        -- equivalence relation of fixity on precedence productions

type UniquenessPair = (PrecedenceProduction, Precedence)

-- This function returns a set of upairs. A upair contains a production of a single precedence on the left,
-- and the set of all productions of that precedence on the right (including the one on the left).
classToPairSet :: Precedence -> Set UniquenessPair
classToPairSet = groupSetBy preceq >. Set.map pair
  where
    pair :: Precedence -> UniquenessPair
    pair prec = (Set.elemAt 0 prec, prec)
    preceq :: PrecedenceProduction -> PrecedenceProduction -> Bool
    preceq a b = prec a == prec b

pairifyClasses :: Set Precedence -> Set (Set UniquenessPair)
pairifyClasses = Set.map classToPairSet

type PqQuad = (Int, Int, PrecedenceProduction, Precedence)

pqboundUPair :: Set UniquenessPair -> Set UniquenessPair -> UniquenessPair -> PqQuad
pqboundUPair pre post (r, s) = (greater pre $ prec r, greater post $ prec r, r, s)
  where
    greater :: Set UniquenessPair -> Int -> Int
    greater upairs n = Set.size $ Set.filter ((n <) . prec . fst) upairs

pqboundClasses :: Set UniquenessPair -> Set UniquenessPair -> Set (Set UniquenessPair) -> Set (Set PqQuad)
pqboundClasses pre post = Set.map (Set.map (pqboundUPair pre post))

intersperseStart :: NonEmpty String -> CfgString
intersperseStart = DLNe.map (Left . Terminal) >. DLNe.intersperse (Right (NonTerminal "!start")) >. DLNe.toList

fill :: Precedence -> Set CfgProduction -> Set CfgProduction
fill s cfgprods = Set.union withTerminals withoutTerminals
  where
    (left, withoutTerminals) = Set.partition hasTerminal cfgprods
      where
        hasTerminal :: CfgProduction -> Bool
        hasTerminal (_, words) = List.any isTerminal words
    isTerminal :: Either Terminal NonTerminal -> Bool
    isTerminal (Right (NonTerminal _)) = False
    isTerminal (Left (Terminal _)) = True
    withTerminals = fill' s left
        -- TODO: write a proper implementation of this composition that doesn't depend on List
      where
        fill' :: Precedence -> Set CfgProduction -> Set CfgProduction
        fill' s = Set.toList >. repeat >. zipWith reset (Set.toList s) >. concat >. Set.fromList
          where
            reset :: PrecedenceProduction -> [CfgProduction] -> [CfgProduction]
            reset pp = map fn
              where
                fn :: CfgProduction -> CfgProduction
                fn (x, rhs) = (x, re rhs)
                  where
                    re :: CfgString -> CfgString
                    re str =
                        case pp of
                            Infixl prec words -> kansas str words
                            Infixr prec words -> kansas str words
                            _ ->
                                error
                                    "This is a bug in Aasam. Somehow, I got a CfgProduction that hasn't any terminals, or a Closed production."
                      where
                        kansas :: CfgString -> NonEmpty String -> CfgString
                        kansas str words = List.head str : intersperseStart words ++ [List.last str]

-- The CE production on `closedrule` must go to a non-terminal.
-- Relevant terminals in these rules are all added by `fill`. Those added immediately in the rule bodies are just to signal to fill.
--   If an "evil" non-terminal appears anywhere in the output of a *rule fuctions, that's a bug. Fix it by making the rules do what the paper says directly.
prerule :: Int -> Int -> PqQuad -> Set CfgProduction
prerule p q (_, _, r, s) = fill s $ Set.singleton (nt (prec r) p q, [Right (nt (prec r - 1) (p + 1) q)])

postrule :: Int -> Int -> PqQuad -> Set CfgProduction
postrule p q (_, _, r, s) = fill s $ Set.singleton (nt (prec r) p q, [Right (nt (prec r - 1) p (q + 1))])

inlrule :: Int -> Int -> PqQuad -> Set CfgProduction
inlrule p q (_, _, r, s) = fill s $ Set.fromList [a, b]
  where
    a = (nt (prec r) p q, [Right (nt (prec r) 0 q), Left (Terminal "evil"), Right (nt (prec r - 1) p 0)])
    b = (nt (prec r) p q, [Right (nt (prec r - 1) p q)])

inrrule :: Int -> Int -> PqQuad -> Set CfgProduction
inrrule p q (_, _, r, s) = fill s $ Set.fromList [a, b]
  where
    a = (nt (prec r) p q, [Right (nt (prec r - 1) 0 q), Left (Terminal "evil"), Right (nt (prec r) p 0)])
    b = (nt (prec r) p q, [Right (nt (prec r - 1) p q)])

closedrule :: Set UniquenessPair -> Set UniquenessPair -> Int -> Int -> PqQuad -> Set CfgProduction
closedrule pres posts p q (_, _, r, s) = insert ae isets `union` jsets
  where
    ae :: CfgProduction
    ae = (nt 0 p q, [Right (NonTerminal "CE")])
    isets :: Set CfgProduction
    isets = foldl (flip (union . ido)) Set.empty (zip (Set.toList pres) [1 .. p])
      where
        ido :: (UniquenessPair, Int) -> Set CfgProduction
        ido ((r, s), i) =
            Set.singleton
                (nt 0 p q, intersperseStart (getWords r |> DLNe.fromList) ++ [Right (nt (prec r) (p - i) 0)])
    jsets :: Set CfgProduction
    jsets = foldl (flip (union . jdo)) Set.empty (zip (Set.toList posts) [1 .. q])
      where
        jdo :: (UniquenessPair, Int) -> Set CfgProduction
        jdo ((r, s), j) =
            Set.singleton
                (nt 0 p q, Right (nt (prec r) 0 (q - j)) : intersperseStart (getWords r |> DLNe.fromList))

convertClass :: (Int -> Int -> PqQuad -> Set CfgProduction) -> Set PqQuad -> Set CfgProduction
convertClass rule = foldl (flip (union . psets)) Set.empty
  where
    psets (pbound, qbound, r, s) = foldl (flip (union . qsets)) Set.empty [0 .. pbound]
      where
        qsets p = foldl ((. flip (rule p) (pbound, qbound, r, s)) . union) Set.empty [0 .. qbound]

convertClasses :: Set UniquenessPair -> Set UniquenessPair -> Set (Set PqQuad) -> Set CfgProduction
convertClasses pres posts = Set.map convertClassBranching >. foldl union Set.empty
  where
    convertClassBranching :: Set PqQuad -> Set CfgProduction
    convertClassBranching quads = convertClass rule quads
      where
        rule =
            case Set.elemAt 0 quads of
                (_, _, Infixl _ _, _) -> inlrule
                (_, _, Infixr _ _, _) -> inrrule
                (_, _, Prefix _ _, _) -> prerule
                (_, _, Postfix _ _, _) -> postrule
                (_, _, Closed _, _) -> closedrule pres posts

newtype AasamError =
    AasamError [String]
    deriving (Show, Eq, Ord)

m :: Precedence -> Either ContextFree AasamError
m precg =
    if null errors
        then Left (nt highestPrecedence 0 0, addCes (assignStart prods))
        else Right (AasamError errors)
  where
    errors :: [String]
    errors = foldl fn [] [positive, noInitSubseq, noInitWhole, classesPrecDisjoint, precContinue]
      where
        fn :: [String] -> Maybe String -> [String]
        fn a e =
            case e of
                Nothing -> a
                Just err -> err : a
        positive =
            if all fn precg
                then Nothing
                else Just errstr
          where
            fn (Closed _) = True
            fn x = prec x > 0
            errstr = "All precedences must be positive integers."
        noInitSubseq =
            if Set.disjoint initials subsequents
                then Nothing
                else Just errstr
          where
            (initials, subsequents) = foldl fn (Set.empty, Set.empty) precg
              where
                fn (i, s) e = (insert (head words) i, (tail words |> Set.fromList) `union` s)
                  where
                    words = getWords e
            errstr = "No initial word may also be a subsequent word of another production."
        noInitWhole =
            if all fx precg
                then Nothing
                else Just errstr
          where
            fx x = all fy precg
              where
                fy y = getWords x `notPrefixedBy` getWords y || x == y
                  where
                    notPrefixedBy :: Eq a => [a] -> [a] -> Bool
                    notPrefixedBy [] [] = False
                    notPrefixedBy (_:_) [] = False
                    notPrefixedBy [] (_:_) = True
                    notPrefixedBy (x:xs) (y:ys) = x /= y || notPrefixedBy xs ys
            errstr = "No initial sequence of words may also be the whole sequence of another production."
        classesPrecDisjoint =
            if allDisjoint precGroups
                then Nothing
                else Just errstr
          where
            allDisjoint :: Ord a => [Set a] -> Bool
            allDisjoint (x:xs) = all (Set.disjoint x) xs && allDisjoint xs
            allDisjoint [] = True
            precGroups :: [Set Int]
            precGroups = List.map (foldl (flip (insert . prec)) Set.empty) (Set.toList classes)
            errstr = "No precedence of a production of one fixity may also be the precedence of a production of another fixity."
        precContinue =
            if precedences == Set.fromList [lowestPrecedence .. highestPrecedence]
                then Nothing
                else Just errstr
          where
            errstr = "The set of precedences must be the set of integers between 1 and greatest precedence, inclusive."
    classes = makeClasses precg
    upairClasses = pairifyClasses classes
    (pre, post) =
        ( unwrapOr Set.empty $ Data.Foldable.find isPre upairClasses
        , unwrapOr Set.empty $ Data.Foldable.find isPost upairClasses)
      where
        isPre :: Set UniquenessPair -> Bool
        isPre clas =
            case Set.elemAt 0 clas of
                (Prefix _ _, _) -> True
                _ -> False
        isPost clas =
            case Set.elemAt 0 clas of
                (Postfix _ _, _) -> True
                _ -> False
    prods = pqboundClasses pre post upairClasses |> convertClasses pre post
    addCes :: Set CfgProduction -> Set CfgProduction
    addCes = union ces
      where
        ces :: Set CfgProduction
        ces =
            Set.filter isClosed precg |>
            Set.map (\(Closed words) -> (NonTerminal "CE", intersperseStart words))
          where
            isClosed :: PrecedenceProduction -> Bool
            isClosed (Closed _) = True
            isClosed _ = False
    assignStart :: Set CfgProduction -> Set CfgProduction
    assignStart = Set.map $ bimap lhsMap rhsMap
      where
        lhsMap :: NonTerminal -> NonTerminal
        lhsMap lhs =
            if lhs == NonTerminal "!start"
                then nt highestPrecedence 0 0
                else lhs
        rhsMap = map submap
          where
            submap :: Either Terminal NonTerminal -> Either Terminal NonTerminal
            submap (Right x) = Right $ lhsMap x
            submap y = y
    (highestPrecedence, lowestPrecedence, precedences) =
        foldl
            (\(ha, la, pa) e -> (max (prec e) ha, min (prec e) la, prec e `insert` pa))
            (0, 0, Set.empty)
            precg
