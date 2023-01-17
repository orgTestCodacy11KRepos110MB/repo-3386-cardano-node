{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Testnet.Conf
  ( ProjectBase(..)
  , YamlFilePath(..)
  , TmpPath(..)
  , Conf(..)
  , mkConf
  ) where

import           Control.Monad
import           Data.Eq
import           Data.Function
import           Data.Int
import           Data.Maybe
import           System.FilePath.Posix ((</>))
import           System.IO (FilePath)
import           Text.Show

import qualified Hedgehog.Extras.Test.Base as H
import qualified System.FilePath.Posix as FP
import qualified System.Random as IO

newtype ProjectBase = ProjectBase
  { projectBase :: FilePath
  } deriving (Eq, Show)

newtype YamlFilePath = YamlFilePath
  { projectBase :: FilePath
  } deriving (Eq, Show)

newtype TmpPath = TmpPath
  { unTmpPath :: FilePath
  } deriving (Eq, Show)

-- type Conf = 

data Conf = Conf
  { tempAbsPath :: FilePath
  , tempBaseAbsPath :: FilePath
  , logDir :: FilePath
  , base :: FilePath
  , socketDir :: FilePath
  , configurationTemplate :: FilePath
  , testnetMagic :: Int
  } deriving (Eq, Show)

mkConf :: ProjectBase -> YamlFilePath -> TmpPath -> Maybe Int -> H.Integration Conf
mkConf (ProjectBase base) (YamlFilePath configurationTemplate) tmpPath maybeMagic = do
  testnetMagic <- maybe (IO.randomRIO (1000, 2000)) return maybeMagic
  let
    tempAbsPath = unTmpPath tmpPath
    tempBaseAbsPath = getTmpBaseAbsPath tmpPath
    tempRelPath = getTmpRelPath tmpPath
    socketDir = getSocketDir tmpPath
    logDir = getLogDir tmpPath

  H.noteShow_ testnetMagic
  H.noteShow_ tempBaseAbsPath
  H.noteShow_ tempRelPath
  H.noteShow_ socketDir
  H.noteShow_ logDir
  return $ Conf {..}

getTmpBaseAbsPath :: TmpPath -> FilePath
getTmpBaseAbsPath (TmpPath fp) = FP.takeDirectory fp

getTmpRelPath :: TmpPath -> FilePath
getTmpRelPath (TmpPath fp) = FP.makeRelative (getTmpBaseAbsPath (TmpPath fp)) fp 

getSocketDir :: TmpPath -> FilePath
getSocketDir fp = getTmpRelPath fp </> "socket"

getLogDir :: TmpPath -> FilePath
getLogDir (TmpPath fp) = fp </> "logs"
