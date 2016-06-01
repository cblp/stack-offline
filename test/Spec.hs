{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad                 (unless, when)
import Data.Monoid                   ((<>))
import Data.String.Interpolate       (i)
import Data.Tuple.Operator           ((:-), pattern (:-))
import Development.Shake             (Action, liftIO, shakeArgs, shakeOptions)
import Development.Shake.Classes     (Binary, Hashable, NFData)
import Development.Shake.Rule.Simple (Rule, need, rule, storedValue, want)
import GHC.Generics                  (Generic)
import System.Directory              (getCurrentDirectory)
import System.Environment            (lookupEnv)
import System.IO                     (hPutStrLn, stderr)
import System.Process                (callProcess)

import Stack.Offline (Snapshot)

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
instance Rule FullCycleTest () where
    storedValue _ _ = pure Nothing

newtype FullCycle = FullCycle Conf
    deriving (Binary, Eq, Generic, Hashable, NFData, Show)
instance Rule FullCycle () where
    storedValue _ _ = pure Nothing

data DockerImage = DockerImage{image :: String, dockerfile :: String}
    deriving (Binary, Eq, Generic, Hashable, NFData, Show)
instance Rule DockerImage () where
    storedValue _ _ = pure Nothing

newtype StackOfflinePack = StackOfflinePack Conf
    deriving (Binary, Eq, Generic, Hashable, NFData, Show)
instance Rule StackOfflinePack () where
    storedValue _ _ = pure Nothing

data Tool = Tool Arch Os
    deriving (Binary, Eq, Generic, Hashable, NFData, Show)
instance Rule Tool () where
    storedValue _ _ = pure Nothing

main :: IO ()
main = do
    runFullTest <- (Just "FULL" ==) <$> lookupEnv "STACK_OFFLINE_TEST"
    unless runFullTest $
        hPutStrLn stderr "Full cycle tests are skipped"

    shakeArgs shakeOptions $ do
        when runFullTest $
            want [FullCycleTest]

        rule $ \FullCycleTest -> Just .
            need $ fmap FullCycle confs

        rule $ \(FullCycle conf) -> Just $
            need [StackOfflinePack conf]

        rule $ \dockerImage -> Just .
            liftIO $ dockerBuild dockerImage

        rule $ \(StackOfflinePack conf) -> Just $
            buildPack conf

        rule $ \tool -> Just $
            buildTool tool

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
    liftIO $ dockerRunUser sourceImage [cwd :- "/opt/stack-offline"] [i|
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
    liftIO $ dockerRunUser sourceImage [cwd :- "/opt/stack-offline"] [i|
        set -x
        rm -f "#{packFile}"
        tmp/bin/stack-offline --resolver="#{snapshot}" --tgz="#{packFile}"
        test -f "#{packFile}"
    |]

dockerBuild :: DockerImage -> IO ()
dockerBuild DockerImage{image, dockerfile} = do
    mHttpProxy <- lookupEnv "http_proxy"
    callProcess "docker" $ concat
        [ [ "build" ]
        , [ "--build-arg=http_proxy=" <> http_proxy | Just http_proxy <- [mHttpProxy] ]
        , [ "--file=" <> dockerfile
          , "--tag=" <> image
          , "./docker/" ]
        ]

-- | Run command in docker container with user privileges
dockerRunUser :: String -> [String :- String] -> String -> IO ()
dockerRunUser image volumes userCommand =
    callProcess "docker" $ concat
        [ [ "run"
          , "--interactive"
          , "--rm"
          -- , "--tty" TODO(cblp, 2016-05-29) try inherit stdin
          , "--workdir=/opt/stack-offline" ]
        , [ "--volume=" <> host <> ":" <> guest | host :- guest <- volumes ]
        , [ image
          , "sudo", "--preserve-env", "--set-home", "--user=user"
          , "bash", "-eu", "-o", "pipefail", "-c"
          , userCommand ]
        ]
