{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

import Data.Monoid          ((<>))
import Data.Traversable     (for)
import Distribution.Package (PackageId)
import Distribution.Text    (simpleParse)
import Options.Applicative  (execParser, fullDesc, help, helper, info, long, metavar, optional,
                             progDesc, strOption)

import Stack.Offline (Config(..), Packages(..), Snapshot)

data Options = Options { options_resolver :: Snapshot, options_packagesFromFile :: Maybe FilePath }
    deriving Show

main :: IO ()
main = parseOptions >>= resolveConfig >>= print
  where
    parseOptions = execParser $ info (helper <*> options) programDescription
    programDescription = mconcat
        [ fullDesc
        , progDesc "Create portable environment sufficient to build your stack project offline" ]
    options = Options
        <$> strOption (mconcat [long "resolver", metavar "SNAPSHOT", help "Snapshot to use"])
        <*> optional (
              strOption $ mconcat
                [ long "packages-from"
                , metavar "FILE"
                , help $ unwords
                    [ "Read packages list from file."
                    , "Each line of the file must be in pkgid format (<name>-<version>)." ] ] )

resolveConfig :: Options -> IO Config
resolveConfig Options{options_resolver, options_packagesFromFile} = do
    config_packages <- case options_packagesFromFile of
        Nothing -> pure PackagesFromShapshot
        Just filePath -> Packages <$> loadPackageList filePath
    pure Config {config_snapshot = options_resolver, config_packages}

loadPackageList :: FilePath -> IO [PackageId]
loadPackageList filePath = do
    contents <- readFile filePath
    for (zip [1 :: Int ..] $ lines contents) $ \(n, line) ->
        case simpleParse line of
            Just pkgid -> pure pkgid
            Nothing -> error $ filePath <> ":" <> show n <> ": cannot parse PackageId"
