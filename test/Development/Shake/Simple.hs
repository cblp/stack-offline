{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Development.Shake.Simple
    (Rule, need, rule, simpleRule, simpleStoredValue, storedValue, want) where

import Data.Functor           (($>))
import Development.Shake      (Action, Rules, ShakeOptions, action)
import Development.Shake.Rule (Rule, apply, rule, storedValue)

class SimpleRule key where
    need :: [key] -> Action ()

instance Rule key () => SimpleRule key where
    need keys = (apply keys :: Action [()]) $> ()

want :: SimpleRule key => [key] -> Rules ()
want = action . need

simpleStoredValue :: Rule key value => ShakeOptions -> key -> IO (Maybe value)
simpleStoredValue _ _ = pure Nothing

simpleRule :: Rule key value => (key -> Action value) -> Rules ()
simpleRule r = rule $ Just . r
