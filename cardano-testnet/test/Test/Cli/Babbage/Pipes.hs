{-# LANGUAGE CPP #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Test.Cli.Babbage.Pipes
  ( hprop_pipes
  ) where

import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Monoid (Last (..))
import           Hedgehog (Property)
import           Prelude
import           System.Environment (getEnvironment)
import           System.FilePath ((</>))
import           System.IO (hClose, hFlush, hPutStr)

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Directory as IO
import qualified System.Info as SYS
import           Testnet ( TestnetOptions (BabbageOnlyTestnetOptions), testnet)
import           Testnet.Babbage (BabbageTestnetOptions (..))
import qualified Testnet.Babbage as TC
import qualified Testnet.Conf as H
import qualified Util.Base as H
import qualified Util.Process as H
import qualified Util.Runtime as TR
#ifdef UNIX
import           System.Posix.IO (closeFd, createPipe, fdToHandle)
#endif

hprop_pipes :: Property
#ifdef UNIX
hprop_pipes = H.integration . H.runFinallies . H.workspace "babbage" $ \tempAbsBasePath' -> do
  H.note_ SYS.os
  base <- H.note =<< H.noteIO . IO.canonicalizePath =<< H.getProjectBase
  configurationTemplate <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"
  conf@H.Conf { H.tempBaseAbsPath, H.tempAbsPath } <- H.noteShowM $
    H.mkConf (H.ProjectBase base) (H.YamlFilePath configurationTemplate) tempAbsBasePath' Nothing

  work <- H.note $ tempAbsPath </> "work"
  H.createDirectoryIfMissing work
  let
    testnetOptions = BabbageOnlyTestnetOptions $ TC.defaultTestnetOptions
      { nodeLoggingFormat = TR.NodeLoggingFormatAsJson
      }
  TR.TestnetRuntime{ poolNodes } <- testnet testnetOptions conf

  poolNode1 <- H.headM poolNodes

  env <- H.evalIO getEnvironment

  poolSprocket1 <- H.noteShow $ TR.nodeSprocket $ TR.poolRuntime poolNode1

  execConfig <- H.noteShow H.ExecConfig
    { H.execConfigEnv = Last $ Just $
      [ ("CARDANO_NODE_SOCKET_PATH", IO.sprocketArgumentName poolSprocket1)
      ]
      -- The environment must be passed onto child process on Windows in order to
      -- successfully start that process.
      <> env
    , H.execConfigCwd = Last $ Just tempBaseAbsPath
    }

  let tx = unlines
        [ "{"
        , "\"type\": \"Unwitnessed Tx BabbageEra\","
        , "\"description\": \"Ledger Cddl Format\","
        , "\"cborHex\": \"84a300818258208a5a31ae52cdc140b1c532c47d5cb3c8dbc6c9b7a4aa537ed9e3543ea94cfad1000183a200581d60768d2bcd29a84ae015d96118e3285bc0cbd399ae16039ad471c8a806011a001e8480a200581d60768d2bcd29a84ae015d96118e3285bc0cbd399ae16039ad471c8a806011a002625a0a200581d600ac4d3db1163e7884959c0317aa19263a236db113fa0fbefbd49b335011a00958d31021a0003094fa0f5f6\""
        , "}"
        ]

  withPipe tx $ \path -> do
    void $ H.execCli' execConfig
      [ "transaction", "txid"
      , "--tx-file", path
      ]

withPipe :: (MonadIO m) => String -> (FilePath -> m a) -> m a
withPipe contents k = do
  (readEnd, writeEnd) <- liftIO createPipe
  liftIO $ do
    writeHandle <- fdToHandle writeEnd
    hPutStr writeHandle contents
    hFlush writeHandle
    hClose writeHandle
  res <- k $ "/dev/fd/" ++ show readEnd
  liftIO $ closeFd readEnd
  pure res

#else
hprop_pipes = H.property $ pure ()
#endif
