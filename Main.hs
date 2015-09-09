
module Main where

import Options.Applicative
import Keter.Pkg
import Keter.Options

main :: IO ()
main = execParser opts
       >>= keterPkg
  where
    opts = info (helper <*> keterPkgOpts)
            ( fullDesc
            <> progDesc "Create Keter app bundles"
            <> header "keter-pkg - create keter bundles" )
