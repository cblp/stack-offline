{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad             (unless, when)
import Data.String.Interpolate   (i)
import Data.Tuple.Operator       (pattern (:-))
import Development.Shake         (Action, liftIO, shakeArgs, shakeOptions)
import Development.Shake.Classes (Binary, Hashable, NFData)
import Development.Shake.Simple  (need, simpleRule, want)
import GHC.Generics              (Generic)
import System.Directory          (getCurrentDirectory)
import System.Environment        (lookupEnv)
import System.IO                 (hPutStrLn, stderr)

import Stack.Offline (Snapshot)

import Docker (DockerImage(..), DockerRun(..), dockerBuild, dockerRunUser)

data Arch = X86_64
    deriving (Binary, Eq, Generic, Hashable, NFData)
instance Show Arch where
    show X86_64 = "x86_64"

data Os = Linux
    deriving (Binary, Eq, Generic, Hashable, NFData)
instance Show Os where
    show Linux = "linux"

-- | Full cycle test configuration
data Conf = Conf{source :: (Arch, Os), snapshot :: Snapshot}
    deriving (Binary, Eq, Generic, Show, Hashable, NFData)

data FullCycleTest = FullCycleTest
    deriving (Binary, Eq, Generic, Hashable, NFData, Show)

newtype FullCycle = FullCycle Conf
    deriving (Binary, Eq, Generic, Hashable, NFData, Show)

newtype StackOfflinePack = StackOfflinePack Conf
    deriving (Binary, Eq, Generic, Hashable, NFData, Show)

data Tool = Tool Arch Os
    deriving (Binary, Eq, Generic, Hashable, NFData, Show)

main :: IO ()
main = do
    runFullTest <- (Just "FULL" ==) <$> lookupEnv "STACK_OFFLINE_TEST"
    unless runFullTest $
        hPutStrLn stderr "Full cycle tests are skipped"

    shakeArgs shakeOptions $ do
        when runFullTest $
            want [FullCycleTest]

        simpleRule $ \FullCycleTest ->
            need $ fmap FullCycle confs

        simpleRule $ \(FullCycle conf) ->
            need [StackOfflinePack conf]

        simpleRule $ -- DockerImage
            liftIO . dockerBuild

        simpleRule $ \(StackOfflinePack conf) ->
            buildPack conf

        simpleRule -- Tool
            buildTool

  where
    confs = [ Conf{source, snapshot}
            | source <- [(X86_64, Linux)]
            , snapshot <- [ "lts-2.0" -- ghc-7.8.4
                          , "lts-3.0" -- ghc-7.10.2
                          , "lts-5.0" -- ghc-7.10.3
                          ]
            ]

dockerImageName :: String -> (Arch, Os) -> String
dockerImageName prefix (arch, os) = [i|stack-offline.#{prefix}.#{arch}-#{os}|]

buildTool :: Tool -> Action ()
buildTool (Tool arch os) = do
    let sourceDockerfile = [i|docker/source.#{arch}-#{os}|]
        sourceImage = dockerImageName "source" (arch, os)
    need [DockerImage sourceImage sourceDockerfile]

    cwd <- liftIO getCurrentDirectory
    liftIO $ dockerRunUser (mkDROptions sourceImage cwd) [i|
        cwd=`pwd`
        mkdir -p tmp/bin
        set -x
        stack --install-ghc --local-bin-path="tmp/bin" --stack-root="$cwd/tmp/stack" install
    |]

buildPack :: Conf -> Action ()
buildPack Conf{source = source@(sourceArch, sourceOs), snapshot} = do
    let sourceDockerfile = [i|docker/source.#{sourceArch}-#{sourceOs}|]
        sourceImage = dockerImageName "source" source
    need [DockerImage sourceImage sourceDockerfile]
    need [Tool sourceArch sourceOs]

    cwd <- liftIO getCurrentDirectory
    let packFile = [i|tmp/stack-offline-pack_#{snapshot}_#{sourceArch}-#{sourceOs}.tgz|]
    liftIO $ dockerRunUser (mkDROptions sourceImage cwd) [i|
        set -x
        rm -f "#{packFile}"
        tmp/bin/stack-offline --resolver="#{snapshot}" --tgz="#{packFile}"
        test -f "#{packFile}"
    |]

mkDROptions :: String -> FilePath -> DockerRun
mkDROptions image cwd = DockerRun
    { dr_image = image
    , dr_volumes = [cwd :- "/opt/stack-offline"]
    , dr_workdir = "/opt/stack-offline"
    }
