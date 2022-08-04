module Util where

infixl 5 >.
(>.) :: (a -> b) -> (b -> c) -> a -> c
(>.) = flip (.)

infixl 4 |>
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

unwrapOr :: Maybe a -> a -> a
unwrapOr (Just x) _ = x
unwrapOr _ y = y

tup :: a -> b -> (a, b)
tup a b = (a, b)