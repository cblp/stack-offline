{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module Docker (DockerImage(..), DockerRun(..), dockerBuild, dockerRunUser) where

import Data.Monoid               ((<>))
import Data.Tuple.Operator       ((:-), pattern (:-))
import Development.Shake.Classes (Binary, Hashable, NFData)
import Development.Shake.Simple  (Rule, simpleStoredValue, storedValue)
import GHC.Generics              (Generic)
import System.Environment        (lookupEnv)
import System.Process            (callProcess)

data DockerImage = DockerImage{di_tag :: String, di_dockerfile :: String}
    deriving (Binary, Eq, Generic, Hashable, NFData, Show)
instance Rule DockerImage () where
    storedValue = simpleStoredValue

dockerBuild :: DockerImage -> IO ()
dockerBuild DockerImage{di_tag, di_dockerfile} = do
    mHttpProxy <- lookupEnv "http_proxy"
    callProcess "docker" $ concat
        [ [ "build" ]
        , [ "--build-arg=http_proxy=" <> http_proxy | Just http_proxy <- [mHttpProxy] ]
        , [ "--file=" <> di_dockerfile
          , "--tag=" <> di_tag
          , "./docker/" ]
        ]

data DockerRun = DockerRun
    {dr_image :: String, dr_volumes :: [String :- String], dr_workdir :: FilePath}

-- | Run command in docker container with user privileges
dockerRunUser :: DockerRun -> String -> IO ()
dockerRunUser DockerRun{dr_image, dr_volumes, dr_workdir} userCommand =
    callProcess "docker" $ concat
        [ [ "run"
          , "--interactive"
          , "--rm"
          -- , "--tty" TODO(cblp, 2016-05-29) try inherit stdin
          , "--workdir=" <> dr_workdir ]
        , [ "--volume=" <> host <> ":" <> guest | host :- guest <- dr_volumes ]
        , [ dr_image
          , "sudo", "--preserve-env", "--set-home", "--user=user"
          , "bash", "-eu", "-o", "pipefail", "-c"
          , userCommand ]
        ]
