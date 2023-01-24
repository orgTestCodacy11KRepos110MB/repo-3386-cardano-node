module Testnet.Util.Cli
  ( cliAddressKeyGen
  , cliNodeKeyGen
  , cliNodeKeyGenVrf
  , cliNodeKeyGenKes
  , cliStakeAddressKeyGen  
  , Comment (..)
  , KeyNames (..)

  , fakeItH
  , fakeIt
  , File (..)
  , Address
  , Operator
  , Kes
  , StakeAddress
  , Vrf

  , VKey
  , SKey

  , OperatorCounter
  , ByronKey
  , ByronAddress
  
  , ByronDelegationKey
  , ByronDelegationCert
  
  , getVKeyPath
  , getSKeyPath

  , makeFilePath
  , cliKeyGen

  , cliSigningKeyAddress
  ) where

import           Prelude

import           Data.String
import           Control.Monad
import           System.FilePath.Posix

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H (writeFile)
import           Hedgehog
import           Hedgehog.Gen

import           Testnet.Util.Process

-- TODO: check no whitespace , slash etc allowed
newtype Comment = Comment { unComment :: String}
  deriving (Show, Eq, Ord )

instance IsString Comment where fromString a = Comment a

instance Semigroup Comment where
  (Comment a) <> (Comment b) = Comment $ a <> "-" <> b

-- Could also use IO (timestamp).
makeFilePath :: MonadGen m => FilePath -> Comment -> FilePath -> m FilePath
makeFilePath path comment suffix = do
  almostUnique <- Comment <$> replicateM 4 hexit
  return $ path </> unComment ( comment <> almostUnique) <.> suffix

data KeyNames = KeyNames
  { verificationKeyFile :: FilePath
  , signingKeyFile :: FilePath
  }

type KeyGen a = H.Integration (File (a VKey), File (a SKey))

-- cliAddressKeyGen :: FilePath -> Comment -> KeyGen
cliAddressKeyGen :: FilePath -> KeyNames -> KeyGen Address
cliAddressKeyGen = shelleyKeyGen "address" "key-gen"

cliStakeAddressKeyGen :: FilePath -> KeyNames -> KeyGen StakeAddress
cliStakeAddressKeyGen = shelleyKeyGen "stake-address" "key-gen"

cliNodeKeyGenVrf :: FilePath -> KeyNames -> KeyGen Vrf
cliNodeKeyGenVrf = shelleyKeyGen "node" "key-gen-VRF"

cliNodeKeyGenKes :: FilePath -> KeyNames -> KeyGen Kes
cliNodeKeyGenKes = shelleyKeyGen "node" "key-gen-KES"

shelleyKeyGen :: String -> String -> FilePath -> KeyNames -> KeyGen x
shelleyKeyGen major minor basePath keyNames = do
  let
    vKeyPath = basePath </> verificationKeyFile keyNames
    sKeyPath = basePath </> signingKeyFile keyNames
  execCli_
      [ major, minor
      , "--verification-key-file", vKeyPath
      , "--signing-key-file", sKeyPath
      ]
  return (File vKeyPath, File sKeyPath)

cliNodeKeyGen
  :: FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> H.Integration (File (Operator VKey), File (Operator SKey), File OperatorCounter)

cliNodeKeyGen base vkey skey counter = do
  let
    vkPath = base </> vkey
    skPath = base </> skey
    counterPath = base </> counter
  execCli_
    [ "node", "key-gen"
    , "--cold-verification-key-file", vkPath
    , "--cold-signing-key-file", skPath
    , "--operational-certificate-issue-counter-file", counterPath
    ]
  return (File vkPath, File skPath, File counterPath)

data Address t
data Kes t
data Operator t
data StakeAddress t
data Vrf t

data VKey
data SKey

data OperatorCounter
--data ByronGenesisKey
--data ByronGenesisAddress
data ByronKey
data ByronAddress
data ByronDelegationKey
data ByronDelegationCert

newtype File a = File {unFile :: FilePath}
  deriving (Show, Eq)

-- When all uses of fakeItH are removed the cleanup is done
fakeItH :: FilePath -> H.Integration (File a)
fakeItH filePath = do
  return $ File filePath
  {-
  exists <- liftIO $ fileExists filePath
  if exists
    then return $ File filePath
    else error (filePath <> " does not exist.")
-}
  
fakeIt :: FilePath -> File a
fakeIt = File

getVKeyPath ::  (File (a VKey), File (a SKey)) -> FilePath
getVKeyPath (File a, _ ) = a

getSKeyPath ::  (File (a VKey), File (a SKey)) -> FilePath
getSKeyPath (_, File a) = a

--byron
cliKeyGen :: FilePath -> FilePath -> H.Integration (File ByronKey)
cliKeyGen tmp key = do
  let keyPath = tmp </> key
  execCli_
      [ "keygen"
      , "--secret", keyPath
      ]
  return $ File keyPath

cliSigningKeyAddress
  :: FilePath
  -> Int
  -> File ByronKey
  -> FilePath
  -> H.Integration (File ByronAddress)
cliSigningKeyAddress tmp testnetMagic (File key) destPath = do
  let addrPath = tmp </> destPath
  addr <- execCli
      [ "signing-key-address"
      , "--testnet-magic", show testnetMagic
      , "--secret", tmp </> key
      ]
  H.writeFile addrPath addr
  return $ File addrPath
