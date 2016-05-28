{-# LANGUAGE TypeOperators #-}

module Stack.Offline (Config(..), Packages(..), Snapshot) where

import Distribution.Package (PackageId)

type Snapshot = String

data Packages = Packages [PackageId] | PackagesFromShapshot
    deriving Show

data Config = Config { config_snapshot :: Snapshot, config_packages :: Packages }
    deriving Show
