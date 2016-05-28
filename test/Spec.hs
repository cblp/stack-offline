{-# LANGUAGE QuasiQuotes #-}

import Control.Monad           (unless)
import Data.Monoid             ((<>))
import Data.String.Interpolate (i)
import System.Directory        (getCurrentDirectory, getHomeDirectory)
import System.Environment      (lookupEnv)
import System.IO               (hPutStrLn, stderr)
import System.Process          (callProcess)
import Test.Tasty              (defaultMain, testGroup)
import Test.Tasty.HUnit        (testCase)

data Arch = X86_64
instance Show Arch where
    show X86_64 = "x86_64"

data Os = Linux
instance Show Os where
    show Linux = "linux"

main :: IO ()
main = do
    runFullTest <- (Just "FULL" ==) <$> lookupEnv "STACK_OFFLINE_TEST"
    unless runFullTest $
        hPutStrLn stderr "Full cycle tests are skipped"
    defaultMain $ testGroup "full cycle" [testCase (show conf) $ fullCycle conf | conf <- confs]
  where
    confs = [(X86_64, Linux, X86_64, Linux)]

fullCycle :: (Arch, Os, Arch, Os) -> IO ()
fullCycle (sourceArch, sourceOs, _targetArch, _targetOs) = do
    let sourceImage = [i|stack-offline.source.#{sourceArch}-#{sourceOs}|]
        sourceDockerfile = [i|docker/source.#{sourceArch}-#{sourceOs}|]
    dockerBuild sourceImage sourceDockerfile
    dockerRunUser sourceImage [i|
        stack setup
        stack install
        stack-offline --help
    |]

dockerBuild :: String -> FilePath -> IO ()
dockerBuild image dockerfile =
    callProcess "docker" ["build", "--file=" <> dockerfile, "--tag=" <> image, "./docker/"]

-- | Run command in docker container with user privileges
dockerRunUser :: String -> String -> IO ()
dockerRunUser image cmd = do
    cwd <- getCurrentDirectory
    home <- getHomeDirectory
    callProcess "docker"
        [ "run"
        , "--interactive"
        , "--rm"
        -- , "--tty" TODO(cblp, 2016-05-29) try inherit stdin
        , [i|--volume=#{cwd}:/opt/stack-offline|] -- TODO(cblp, 2016-05-29) pass volumes as arguments
        , [i|--volume=#{home}/.stack:/home/user/.stack|]
        , "--workdir=/opt/stack-offline"
        , image
        , "sudo", "--set-home", "--user=user"
        , "bash", "-eux", "-o", "pipefail", "-c"
        , cmd
        ]
