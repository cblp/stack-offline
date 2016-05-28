module Stack.Offline (Config(..)) where

data Config = Config { config_snapshot :: String }
    deriving Show
