{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Development.Shake.Rule.Simple (Rule, need, rule, storedValue, want) where

import Data.Functor           (($>))
import Development.Shake      (Action, Rules, action)
import Development.Shake.Rule (Rule, apply, rule, storedValue)

class SimpleRule key where
    need :: [key] -> Action ()

instance Rule key () => SimpleRule key where
    need keys = (apply keys :: Action [()]) $> ()

want :: SimpleRule key => [key] -> Rules ()
want = action . need
