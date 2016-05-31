{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad           (unless)
import Data.List.Extra         (stripPrefix, stripSuffix)
import Data.Monoid             ((<>))
import Data.String.Interpolate (i)
import Data.Tuple.Operator     ((:-), pattern (:-))
import System.Directory        (getCurrentDirectory)
import System.Environment      (lookupEnv)
import System.IO               (hPutStrLn, stderr)
import System.Process          (callProcess)
import Test.Tasty              (defaultMain, testGroup)
import Test.Tasty.HUnit        (testCase)

import Stack.Offline (Snapshot)

showShortSnapshot :: Snapshot -> String
showShortSnapshot (stripPrefix "lts-" -> Just rest) = "L" <> showShortSnapshot rest
showShortSnapshot (stripSuffix ".0" -> Just rest) = showShortSnapshot rest
showShortSnapshot other = other

data Arch = X86_64
instance Show Arch where
    show X86_64 = "x86_64"

showShortArch :: Arch -> String
showShortArch X86_64 = "64"

data Os = Linux
instance Show Os where
    show Linux = "linux"

showShortOs :: Os -> String
showShortOs Linux = "L"

-- | Full cycle test configuration
data Conf = Conf{source :: (Arch, Os), snapshot :: Snapshot}
    deriving Show

showShortConf :: Conf -> String
showShortConf Conf{source = (arch, os), snapshot} =
    mconcat [showShortOs os, showShortArch arch, showShortSnapshot snapshot]

main :: IO ()
main = do
    runFullTest <- (Just "FULL" ==) <$> lookupEnv "STACK_OFFLINE_TEST"
    unless runFullTest $
        hPutStrLn stderr "Full cycle tests are skipped"
    defaultMain $
        if runFullTest then
            testGroup "full cycle" [testCase (show conf) $ fullCycle conf | conf <- confs]
        else
            testGroup "" []
  where
    confs = [ Conf{source, snapshot}
            | source <- [(X86_64, Linux)]
            , snapshot <- ["lts-2.0" {- ghc-7.8.4 -}, "lts-3.0" {- ghc-7.10.2 -}]
            ]

fullCycle :: Conf -> IO ()
fullCycle conf@Conf{source = (sourceArch, sourceOs), snapshot} = do
    let sourceImage = [i|stack-offline.source.#{sourceArch}-#{sourceOs}|]
        sourceDockerfile = [i|docker/source.#{sourceArch}-#{sourceOs}|]
    dockerBuild sourceImage sourceDockerfile

    cwd <- getCurrentDirectory
    let packFile  = [i|tmp/stack-offline-pack_#{snapshot}_#{sourceArch}-#{sourceOs}.tgz|]
        stackRoot = [i|tmp/stack-root_#{showShortConf conf}|]
        stackWork = [i|.stack-work_#{showShortConf conf}|]
    dockerRunUser sourceImage [cwd :- "/opt/stack-offline"] [i|
        export PATH=$HOME/.local/bin:$PATH
        set -x
        stack --install-ghc --stack-root="$(pwd)/#{stackRoot}" --work-dir="#{stackWork}" install
        rm -f "#{packFile}"
        stack-offline --resolver="#{snapshot}" --tgz="#{packFile}"
        test -f "#{packFile}"
    |]

dockerBuild :: String -> FilePath -> IO ()
dockerBuild image dockerfile = do
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
dockerRunUser image volumes cmd =
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
          , cmd ]
        ]
