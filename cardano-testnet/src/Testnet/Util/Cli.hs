module Testnet.Util.Cli
  ( cliAddressKeyGen
  , cliNodeKeyGenVrf
  , cliNodeKeyGenKes
  , cliStakeAddressKeyGen  
  , KeyNames (..)
  ) where

import           Prelude

import           System.FilePath.Posix ((</>))

import qualified Hedgehog.Extras.Test.Base as H
import           Testnet.Util.Process

data KeyNames = KeyNames
  { verificationKeyFile :: FilePath
  , signingKeyFile :: FilePath
  }

type KeyGen = H.Integration ()

cliAddressKeyGen :: FilePath -> KeyNames -> KeyGen
cliAddressKeyGen = cliKeyGen "address" "key-gen"

cliStakeAddressKeyGen :: FilePath -> KeyNames -> KeyGen
cliStakeAddressKeyGen = cliKeyGen "stake-address" "key-gen"

cliNodeKeyGenVrf :: FilePath -> KeyNames -> KeyGen
cliNodeKeyGenVrf = cliKeyGen "node" "key-gen-VRF"

cliNodeKeyGenKes :: FilePath -> KeyNames -> KeyGen
cliNodeKeyGenKes = cliKeyGen "node" "key-gen-KES"

cliKeyGen :: String -> String -> FilePath -> KeyNames -> KeyGen
cliKeyGen major minor basePath keyNames = do
  execCli_
      [ major, minor
      , "--verification-key-file", basePath </> verificationKeyFile keyNames
      , "--signing-key-file", basePath </> signingKeyFile keyNames
      ]
