{-# LANGUAGE NamedFieldPuns #-}

import Options.Applicative (execParser, fullDesc, help, helper, info, long, metavar, progDesc,
                            strOption)

import Stack.Offline (Config(..))

data Options = Options { options_resolver :: String }
    deriving Show

main :: IO ()
main = execParser programOptions >>= print . mkConfig
  where
    programOptions = info (helper <*> options) $ mconcat
        [ fullDesc
        , progDesc "Create portable environment sufficient to build your stack project offline" ]
    options = Options
        <$> strOption (mconcat [long "resolver", metavar "SNAPSHOT", help "Snapshot to use"])

mkConfig :: Options -> Config
mkConfig Options{options_resolver} = Config{config_snapshot = options_resolver}
