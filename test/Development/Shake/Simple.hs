{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Development.Shake.Simple
    (Rule, need, rule, simpleRule, simpleStoredValue, storedValue, want) where

import Data.Functor              (($>))
import Development.Shake         (Action, Rules, ShakeOptions, ShakeValue, action)
import Development.Shake.Classes (Binary, Hashable, NFData)
import Development.Shake.Rule    (Rule, apply, rule, storedValue)

newtype SimpleKey a = SimpleKey a
    deriving (Binary, Eq, Hashable, NFData, Show)

fromSimpleKey :: SimpleKey a -> a
fromSimpleKey (SimpleKey a) = a

class SimpleRule key where
    need :: [key] -> Action ()

instance ShakeValue key => Rule (SimpleKey key) () where
    storedValue = simpleStoredValue

instance Rule (SimpleKey key) () => SimpleRule key where
    need keys = (apply $ fmap SimpleKey keys :: Action [()]) $> ()

want :: SimpleRule key => [key] -> Rules ()
want = action . need

simpleStoredValue :: Rule key value => ShakeOptions -> key -> IO (Maybe value)
simpleStoredValue _ _ = pure Nothing

simpleRule :: Rule (SimpleKey key) value => (key -> Action value) -> Rules ()
simpleRule r = rule $ Just . r . fromSimpleKey
