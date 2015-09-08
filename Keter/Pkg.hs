
-- | Create Keter Bundles
--
-- Original module was adapted from 'yesod-bin' in order to
-- make an executable more suitable for keter-specific
-- features and automation.

{-# LANGUAGE OverloadedStrings #-}

module Keter.Pkg
    (
      keterPkg
    ) where

import qualified Codec.Archive.Tar          as Tar
import qualified Data.ByteString.Lazy       as L
import qualified Data.Foldable              as Fold
import qualified Data.HashMap.Strict        as Map
import qualified Data.Text                  as T


import Control.Applicative
import Codec.Compression.GZip     (compress)
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Writer (execWriter, tell)
import Data.Maybe
import Data.Yaml
import System.Directory           hiding (findFiles)
import System.Exit
import System.FilePath            ((</>))
import System.Process

import Keter.Options


keterPkg :: KeterPkgOpts-> IO ()
keterPkg (KeterPkgOpts kcfg _ copyto) = do
  keter kcfg copyto


run :: String -> [String] -> IO ()
run a b = do
    ec <- rawSystem a b
    unless (ec == ExitSuccess) $ exitWith ec


try' :: IO a -> IO (Either SomeException a)
try' = try


keter :: Maybe FilePath -- ^ Keter config file
      -> Bool             -- ^ no copy to?
      -> IO ()
keter mkcfg noCopyTo = do
    ketercfg <- fromMaybe <$> keterConfig <*> pure mkcfg
    mvalue <- decodeFile ketercfg
    value <-
        case mvalue of
            Nothing -> error "No config/keter.yaml found"
            Just (Object value) ->
                case Map.lookup "host" value of
                    Just (String s) | "<<" `T.isPrefixOf` s ->
                        error $ "Please set your hostname in " ++ ketercfg
                    _ ->
                        case Map.lookup "user-edited" value of
                            Just (Bool False) ->
                                error $ "Please edit your Keter config file at "
                                     ++ ketercfg
                            _ -> return value
            Just _ -> error $ ketercfg ++ " is not an object"

    files <- getDirectoryContents "."
    project <-
        case mapMaybe (T.stripSuffix ".cabal" . T.pack) files of
            [x] -> return x
            [] -> error "No cabal file found"
            _ -> error "Too many cabal files found"

    let findFiles (Object v) =
            mapM_ go $ Map.toList v
          where
            go ("exec", String s) = tellFile s
            go ("extraFiles", Array a) = Fold.mapM_ tellExtra a
            go (_, v') = findFiles v'
            tellFile s = tell [collapse $ "config" </> T.unpack s]
            tellExtra (String s) = tellFile s
            tellExtra _          = error "extraFiles should be a flat array"
        findFiles (Array v) = Fold.mapM_ findFiles v
        findFiles _ = return ()
        bundleFiles = execWriter $ findFiles $ Object value

        collapse = T.unpack . T.intercalate "/" . collapse' . T.splitOn "/" . T.pack
        collapse' (_:"..":rest) = collapse' rest
        collapse' (".":xs) = collapse' xs
        collapse' (x:xs) = x : collapse' xs
        collapse' [] = []

    _ <- try' $ removeDirectoryRecursive "static/tmp"

    archive <- Tar.pack "" $
        "config" : "static" : bundleFiles
    let fp = T.unpack project ++ ".keter"
    L.writeFile fp $ compress $ Tar.write archive

    unless noCopyTo $ case Map.lookup "copy-to" value of
        Just (String s) ->
            let baseArgs = [fp, T.unpack s] :: [String]

                scpArgs =
                    case parseMaybe (.: "copy-to-args") value of
                        Just as -> as ++ baseArgs
                        Nothing -> baseArgs

                args =
                    case parseMaybe (.: "copy-to-port") value of
                        Just i -> "-P" : show (i :: Int) : scpArgs
                        Nothing -> scpArgs

            in run "scp" args

        _ -> return ()
  where
    -- Test for alternative config file extension (yaml or yml).
    keterConfig = do
        let yml = "config/keter.yml"
        ymlExists <- doesFileExist yml
        return $ if ymlExists then yml else "config/keter.yaml"
