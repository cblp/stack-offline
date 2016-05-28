{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module Data.Tuple.Operator ((:-), pattern (:-)) where

type a :- b = (a, b)

pattern a :- b = (a, b)
infixr 1 :-
