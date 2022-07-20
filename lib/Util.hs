module Util where

infixl 5 >.
(>.) :: (a -> b) -> (b -> c) -> a -> c
(>.) = flip (.)

infixl 4 |>
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

unwrapOr :: a -> Maybe a -> a
unwrapOr _ (Just x) = x
unwrapOr y _ = y