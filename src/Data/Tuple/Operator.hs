{-# LANGUAGE TypeOperators #-}

module Data.Tuple.Operator (type (-:), (-:)) where

type a -: b = (a, b)

(-:) :: a -> b -> (a, b)
a -: b = (a, b)
{-# INLINE (-:) #-}
