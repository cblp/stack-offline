{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

instance ShakeValue key => Rule (SimpleKey key) () where
    storedValue = simpleStoredValue

need :: Rule (SimpleKey key) () => [key] -> Action ()
need keys = (apply $ fmap SimpleKey keys :: Action [()]) $> ()

want :: Rule (SimpleKey key) () => [key] -> Rules ()
want = action . need

simpleStoredValue :: Rule key value => ShakeOptions -> key -> IO (Maybe value)
simpleStoredValue _ _ = pure Nothing

simpleRule :: Rule (SimpleKey key) value => (key -> Action value) -> Rules ()
simpleRule r = rule $ Just . r . fromSimpleKey
