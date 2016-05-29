{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Stack.Offline (Config(..), Packages(..), Snapshot, createPack) where

import Distribution.Package (PackageId)

type Snapshot = String

data Packages = Packages [PackageId] | PackagesFromShapshot
    deriving Show

data Config = Config
    {config_snapshot :: Snapshot, config_packages :: Packages, config_outFile :: FilePath}
    deriving Show

createPack :: Config -> IO ()
createPack Config{config_outFile} =
    writeFile config_outFile ""
