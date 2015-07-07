
module Keter.Options
       (
         KeterPkgOpts(..)
       , keterPkgOpts
       ) where

import Options.Applicative


data KeterPkgOpts = KeterPkgOpts
  {
    -- TODO currently a Maybe FilePath in case it's not
    -- specified. The default is read inside the old keter
    -- code for compatibility but should be refactored to read
    -- the default in the opts parser.
    keterCfg :: !(Maybe FilePath)
  , quiet    :: !Bool
  , copyTo   :: !Bool
  }


-- | keter-pkg options parser
keterPkgOpts :: Parser KeterPkgOpts
keterPkgOpts =
  KeterPkgOpts
  <$> optional
      ( strOption
         ( long "config"
        <> metavar "KETER_CONF"
        <> help "Override location keter config location" ) )
  <*> switch
      ( long "verbose"
     <> help "Turn on verbose logging" )
  <*> switch
      ( long "no-copy"
     <> help "Don't copy the resulting keter bundle to destination using `scp`" )
