{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Testnet.Cardano
  ( ForkPoint(..)
  , CardanoTestnetOptions(..)
  , defaultTestnetOptions
  , TestnetNodeOptions(..)
  , cardanoDefaultTestnetNodeOptions

  , Era(..)
  , TestnetRuntime (..)
  , PaymentKeyPair(..)

  , cardanoTestnet
  ) where

import qualified Cardano.Crypto.Hash.Blake2b
import qualified Cardano.Crypto.Hash.Class
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Except
import           Data.Aeson ((.=))
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy (ByteString)
import           Data.List ((\\))
import           Data.Maybe
import           Data.String
import qualified Hedgehog as H
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))
import           Hedgehog.Extras.Stock.Time (formatIso8601, showUTCTimeSeconds)
import           Ouroboros.Network.PeerSelection.LedgerPeers (UseLedgerAfter (..))
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint (..))
import           Prelude
import           System.FilePath.Posix ((</>))

import           Cardano.Chain.Genesis (GenesisHash (unGenesisHash), readGenesisData)
import qualified Cardano.Node.Configuration.Topology as NonP2P
import qualified Cardano.Node.Configuration.TopologyP2P as P2P
import qualified Data.Aeson as J
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Time.Clock as DTC
import qualified Hedgehog.Extras.Stock.Aeson as J
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Stock.OS as OS
import qualified Hedgehog.Extras.Stock.String as S
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Network as H
import qualified System.Directory as IO
import qualified System.Info as OS
import qualified Testnet.Util.Assert as H
import           Testnet.Util.Cli
import qualified Testnet.Util.Process as H
import           Testnet.Util.Process (execCli_)
import           Testnet.Util.Runtime as TR (NodeLoggingFormat (..), PaymentKeyPair (..),
                   PoolNode (PoolNode), TestnetRuntime (..), startNode)
import           Testnet.Util.Runtime (PoolNodeKeys (..))

import qualified Testnet.Conf as H

{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Use let" -}

data ForkPoint
  = AtVersion Int
  | AtEpoch Int
  deriving (Show, Eq, Read)

data Era = Byron | Shelley | Allegra | Mary | Alonzo deriving (Eq, Enum, Bounded, Read, Show)

data CardanoTestnetOptions = CardanoTestnetOptions
  { -- | List of node options. Each option will result in a single node being
    -- created.
    cardanoBftNodeOptions :: [TestnetNodeOptions]
  , cardanoNumPoolNodes :: Int
  , cardanoEra :: Era
  , cardanoEpochLength :: Int
  , cardanoSlotLength :: Double
  , cardanoActiveSlotsCoeff :: Double
  , cardanoEnableP2P :: Bool
  , cardanoNodeLoggingFormat :: NodeLoggingFormat
  } deriving (Eq, Show)

defaultTestnetOptions :: CardanoTestnetOptions
defaultTestnetOptions = CardanoTestnetOptions
  { cardanoBftNodeOptions = L.replicate 2 cardanoDefaultTestnetNodeOptions
  , cardanoNumPoolNodes = 1
  , cardanoEra = Alonzo
  , cardanoEpochLength = 1500
  , cardanoSlotLength = 0.2
  , cardanoActiveSlotsCoeff = 0.2
  , cardanoEnableP2P = False
  , cardanoNodeLoggingFormat = NodeLoggingFormatAsText
  }

newtype TestnetNodeOptions = TestnetNodeOptions
  { -- | These arguments will be appended to the default set of CLI options when
    -- starting the node.
    extraNodeCliArgs :: [String]
  } deriving (Eq, Show)

cardanoDefaultTestnetNodeOptions :: TestnetNodeOptions
cardanoDefaultTestnetNodeOptions = TestnetNodeOptions
  { extraNodeCliArgs = []
  }

ifaceAddress :: String
ifaceAddress = "127.0.0.1"

-- | For an unknown reason, CLI commands are a lot slower on Windows than on Linux and
-- MacOS.  We need to allow a lot more time to set up a testnet.
startTimeOffsetSeconds :: DTC.NominalDiffTime
startTimeOffsetSeconds = if OS.isWin32 then 90 else 15


mkTopologyConfig :: Int -> [Int] -> Int -> Bool -> ByteString
mkTopologyConfig numNodes allPorts port False = J.encode topologyNonP2P
  where
    topologyNonP2P :: NonP2P.NetworkTopology
    topologyNonP2P =
      NonP2P.RealNodeTopology
        [ NonP2P.RemoteAddress (fromString ifaceAddress)
                               (fromIntegral peerPort)
                               (numNodes - 1)
        | peerPort <- allPorts \\ [port]
        ]
mkTopologyConfig numNodes allPorts port True = J.encode topologyP2P
  where
    rootConfig :: P2P.RootConfig
    rootConfig =
      P2P.RootConfig
        [ RelayAccessAddress (fromString ifaceAddress)
                             (fromIntegral peerPort)
        | peerPort <- allPorts \\ [port]
        ]
        P2P.DoNotAdvertisePeer

    localRootPeerGroups :: P2P.LocalRootPeersGroups
    localRootPeerGroups =
      P2P.LocalRootPeersGroups
        [ P2P.LocalRootPeersGroup rootConfig
                                  (numNodes - 1)
        ]

    topologyP2P :: P2P.NetworkTopology
    topologyP2P =
      P2P.RealNodeTopology
        localRootPeerGroups
        []
        (P2P.UseLedger DontUseLedger)

cardanoTestnet :: CardanoTestnetOptions -> H.Conf -> H.Integration TestnetRuntime
cardanoTestnet testnetOptions configuration@H.Conf {tempAbsPath, testnetMagic} = do
  void $ H.note OS.os
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime startTimeOffsetSeconds currentTime


  ( byronGenesisKeys
    , byronDelegationCerts
    , byronDelegationKeys) <- cardanoTestnetByronGenesis testnetOptions configuration startTime
{-
  cardanoTestnetByronGenesisExpenditure testnetOptions configuration byronGenesisKeys
  cardanoTestnetByronGovernance
    (L.length $ cardanoBftNodeOptions testnetOptions)
    testnetMagic
    tempAbsPath
-}
  cardanoTestnetShelleyGenesis testnetOptions configuration startTime
  poolKeys <- cardanoTestnetPart5 testnetOptions configuration

  wallets <- cardanoTestnetPart6 testnetOptions configuration
  cardanoTestnetPart7 testnetOptions configuration
  testnetRuntime <- cardanoTestnetLaunchCluster testnetOptions configuration poolKeys wallets
  return testnetRuntime

cardanoTestnetByronGenesis
  :: CardanoTestnetOptions
  -> H.Conf
  -> DTC.UTCTime
  -> H.Integration
    ( [File ByronKey]
    , [File ByronDelegationCert]
    , [File ByronDelegationKey]
    )
cardanoTestnetByronGenesis testnetOptions H.Conf {..} startTime = do

  let numBftNodes = L.length (cardanoBftNodeOptions testnetOptions)
      bftNodesN = [1 .. numBftNodes]
      poolNodesN = [1 .. cardanoNumPoolNodes testnetOptions]
      bftNodeNames = ("node-bft" <>) . show @Int <$> bftNodesN
      poolNodeNames = ("node-pool" <>) . show @Int <$> poolNodesN
      allNodeNames = bftNodeNames <> poolNodeNames
      maxByronSupply = 10020000000

  allPorts <- H.noteShowIO $ IO.allocateRandomPorts (L.length allNodeNames)
  nodeToPort <- H.noteShow (M.fromList (L.zip allNodeNames allPorts))

  let securityParam = 10
  configurationFile <- H.noteShow $ tempAbsPath </> "configuration.yaml"
  H.readFile configurationTemplate >>= H.writeFile configurationFile

  forkOptions <- pure $ id
    . HM.insert "EnableLogMetrics" (J.toJSON False)
    . HM.insert "EnableLogging" (J.toJSON True)
    . case cardanoEra testnetOptions of
        Byron -> id
          . HM.insert "LastKnownBlockVersion-Major" (J.toJSON @Int 1)

        Shelley -> id
          . HM.insert "TestShelleyHardForkAtEpoch" (J.toJSON @Int 0)
          . HM.insert "LastKnownBlockVersion-Major" (J.toJSON @Int 2)

        Allegra -> id
          . HM.insert "TestShelleyHardForkAtEpoch" (J.toJSON @Int 0)
          . HM.insert "TestAllegraHardForkAtEpoch" (J.toJSON @Int 0)
          . HM.insert "LastKnownBlockVersion-Major" (J.toJSON @Int 3)

        Mary -> id
          . HM.insert "TestShelleyHardForkAtEpoch" (J.toJSON @Int 0)
          . HM.insert "TestAllegraHardForkAtEpoch" (J.toJSON @Int 0)
          . HM.insert "TestMaryHardForkAtEpoch" (J.toJSON @Int 0)
          . HM.insert "LastKnownBlockVersion-Major" (J.toJSON @Int 4)

        Alonzo -> id
          . HM.insert "TestShelleyHardForkAtEpoch" (J.toJSON @Int 0)
          . HM.insert "TestAllegraHardForkAtEpoch" (J.toJSON @Int 0)
          . HM.insert "TestMaryHardForkAtEpoch" (J.toJSON @Int 0)
          . HM.insert "TestAlonzoHardForkAtEpoch" (J.toJSON @Int 0)
          . HM.insert "LastKnownBlockVersion-Major" (J.toJSON @Int 6)

  -- We're going to use really quick epochs (300 seconds), by using short slots 0.2s
  -- and K=10, but we'll keep long KES periods so we don't have to bother
  -- cycling KES keys
  H.rewriteYamlFile (tempAbsPath </> "configuration.yaml") . J.rewriteObject
    $ HM.insert "Protocol" (J.toJSON @String "Cardano")
    . HM.insert "PBftSignatureThreshold" (J.toJSON @Double 0.6)
    . HM.insert "minSeverity" (J.toJSON @String "Debug")
    . HM.insert "ByronGenesisFile" (J.toJSON @String "byron/genesis.json")
    . HM.insert "ShelleyGenesisFile" (J.toJSON @String "shelley/genesis.json")
    . HM.insert "AlonzoGenesisFile" (J.toJSON @String "shelley/genesis.alonzo.json")
    . HM.insert "RequiresNetworkMagic" (J.toJSON @String "RequiresMagic")
    . HM.insert "LastKnownBlockVersion-Major" (J.toJSON @Int 6)
    . HM.insert "LastKnownBlockVersion-Minor" (J.toJSON @Int 0)
    . HM.insert "TraceBlockchainTime" (J.toJSON True)
    . HM.delete "GenesisFile"
    . HM.insert "TestEnableDevelopmentHardForkEras" (J.toJSON @Bool True)
    . HM.insert "EnableP2P" (J.toJSON @Bool (cardanoEnableP2P testnetOptions))
    . flip HM.alter "setupScribes"
        ( fmap
          . J.rewriteArrayElements
            . J.rewriteObject
              . HM.insert "scFormat"
                $ case cardanoNodeLoggingFormat testnetOptions of
                  NodeLoggingFormatAsJson -> "ScJson"
                  NodeLoggingFormatAsText -> "ScText")
    . forkOptions

  forM_ allNodeNames $ \node -> do
    H.createDirectoryIfMissing $ tempAbsPath </> node
    H.createDirectoryIfMissing $ tempAbsPath </> node </> "byron"
    H.createDirectoryIfMissing $ tempAbsPath </> node </> "shelley"

  -- Make topology files
  forM_ allNodeNames $ \node -> do
    let port = fromJust $ M.lookup node nodeToPort
    H.lbsWriteFile (tempAbsPath </> node </> "topology.json") $
      mkTopologyConfig (numBftNodes + cardanoNumPoolNodes testnetOptions)
                       allPorts port (cardanoEnableP2P testnetOptions)

    H.writeFile (tempAbsPath </> node </> "port") (show port)

  H.lbsWriteFile (tempAbsPath </> "byron.genesis.spec.json") . J.encode $ J.object
    [ "heavyDelThd" .= J.toJSON @String "300000000000"
    , "maxBlockSize" .= J.toJSON @String "2000000"
    , "maxTxSize" .= J.toJSON @String "4096"
    , "maxHeaderSize" .= J.toJSON @String "2000000"
    , "maxProposalSize" .= J.toJSON @String "700"
    , "mpcThd" .= J.toJSON @String "20000000000000"
    , "scriptVersion" .= J.toJSON @Int 0
    , "slotDuration" .= J.toJSON @String "1000"
    , "softforkRule" .= J.object
      [ "initThd" .= J.toJSON @String "900000000000000"
      , "minThd" .= J.toJSON @String "600000000000000"
      , "thdDecrement" .= J.toJSON @String "50000000000000"
      ]
    , "txFeePolicy" .= J.object
      [ "multiplier" .= J.toJSON @String "43946000000"
      , "summand" .= J.toJSON @String "155381000000000"
      ]
    , "unlockStakeEpoch" .= J.toJSON @String "18446744073709551615"
    , "updateImplicit" .= J.toJSON @String "10000"
    , "updateProposalThd" .= J.toJSON @String "100000000000000"
    , "updateVoteThd" .= J.toJSON @String "1000000000000"
    ]

  -- stuff
  execCli_
    [ "byron"
    , "genesis"
    , "genesis"
    , "--protocol-magic", show @Int testnetMagic
    , "--start-time", showUTCTimeSeconds startTime
    , "--k", show @Int securityParam
    , "--n-poor-addresses", "0"
    , "--n-delegate-addresses", show @Int numBftNodes
    , "--total-balance", show @Int maxByronSupply
    , "--delegate-share", "1"
    , "--avvm-entry-count", "0"
    , "--avvm-entry-balance", "0"
    , "--protocol-parameters-file", tempAbsPath </> "byron.genesis.spec.json"
    , "--genesis-output-dir", tempAbsPath </> "byron"
    ]

  H.renameFile
    (tempAbsPath </> "byron.genesis.spec.json")
    (tempAbsPath </> "byron/genesis.spec.json")

  -- Symlink the BFT operator keys from the genesis delegates, for uniformity
  forM_ bftNodesN $ \n -> do
    H.createFileLink (tempAbsPath </> "byron/delegate-keys.00" <> show @Int (n - 1) <> ".key") (tempAbsPath </> "node-bft" <> show @Int n </> "byron/delegate.key")
    H.createFileLink (tempAbsPath </> "byron/delegation-cert.00" <> show @Int (n - 1) <> ".json") (tempAbsPath </> "node-bft" <> show @Int n </> "byron/delegate.cert")
  let
    forallBftNodesMkFile x = forM [1..numBftNodes] (fakeItH . x)

  (genesisKeys :: [File ByronKey])<- forallBftNodesMkFile $ \n -> "byron/genesis-keys.00" <> show @Int (n - 1) <> ".json"
  (delegationCerts :: [File ByronDelegationCert]) <- forallBftNodesMkFile $ \n -> "byron/delegate-keys.00" <> show @Int (n - 1) <> ".key"
  (delegationKeys :: [File ByronDelegationKey]) <- forallBftNodesMkFile $ \n -> "byron/delegation-cert.00" <> show @Int (n - 1) <> ".json"

  return (genesisKeys, delegationCerts, delegationKeys)
  

{-
cardanoTestnetByronGenesisExpenditure creates an transaction `tx0.tx'
which however is never used anywhere ??
-}
cardanoTestnetByronGenesisExpenditure
  :: CardanoTestnetOptions
  -> H.Conf
  -> [File ByronKey]
  -> H.Integration ()
cardanoTestnetByronGenesisExpenditure testnetOptions H.Conf {..} genesisKeys = do
  let numBftNodes = L.length (cardanoBftNodeOptions testnetOptions)

  -- Create keys and addresses to withdraw the initial UTxO into
  byronAddress0:_ <- forM [1 .. numBftNodes] $ \n -> do
    skey <- cliKeyGen tempAbsPath $ "byron/payment-keys.00" <> show @Int (n - 1) <> ".key"
    cliSigningKeyAddress tempAbsPath testnetMagic skey $ "byron/address-00" <> show @Int (n - 1)

  byronGensisAddress0:_ <- forM (zip [1 .. numBftNodes] genesisKeys) $ \(n,key) -> do
    cliSigningKeyAddress tempAbsPath testnetMagic
      key
      ("byron/genesis-address-00" <> show @Int (n - 1))
  
  richAddrFrom <- S.firstLine <$> H.readFile (tempAbsPath </> "byron/genesis-address-000")
  txAddr <- S.firstLine <$> H.readFile (unFile byronAddress0)
  let
      maxByronSupply = 10020000000
      fundsPerGenesisAddress = maxByronSupply `div` numBftNodes
      fundsPerByronAddress = fundsPerGenesisAddress - 100000000

  -- Create Byron address that moves funds out of the genesis UTxO into a regular
  -- address.
  execCli_
    [ "issue-genesis-utxo-expenditure"
    , "--genesis-json", tempAbsPath </> "byron/genesis.json"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx", tempAbsPath </> "tx0.tx"                             -- never transmitted or referenced ?
    , "--wallet-key", tempAbsPath </> "byron/delegate-keys.000.key"
    , "--rich-addr-from", richAddrFrom
    , "--txout", show @(String, Int) (txAddr, fundsPerByronAddress) -- should read FundsPerByronAddress from genesis!
    ]

{-
cardanoTestnetByronGovernance creates two update-proposals and two update-votes.
However they are never transmitted to the chain ?!
-}
cardanoTestnetByronGovernance
  :: Int
  -> Int
  -> FilePath
  -> H.Integration ()
cardanoTestnetByronGovernance numBftNodes testnetMagic tempAbsPath = do

  -- Update Proposal and votes
  execCli_
    [ "byron", "governance", "create-update-proposal"
    , "--filepath", tempAbsPath </> "update-proposal"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key", tempAbsPath </> "byron/delegate-keys.000.key"
    , "--protocol-version-major", "1"
    , "--protocol-version-minor", "0"
    , "--protocol-version-alt", "0"
    , "--application-name", "cardano-sl"
    , "--software-version-num", "1"
    , "--system-tag", "linux"
    , "--installer-hash", "0"
    ]

  forM_ [1 .. numBftNodes] $ \n -> do
    execCli_
      [ "byron", "governance", "create-proposal-vote"
      , "--proposal-filepath", tempAbsPath </> "update-proposal"
      , "--testnet-magic", show @Int testnetMagic
      , "--signing-key", tempAbsPath </> "byron/delegate-keys.00" <> show @Int (n - 1) <> ".key"
      , "--vote-yes"
      , "--output-filepath", tempAbsPath </> "update-vote.00" <> show @Int (n - 1)
      ]

  execCli_
    [ "byron", "governance", "create-update-proposal"
    , "--filepath", tempAbsPath </> "update-proposal-1"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key", tempAbsPath </> "byron/delegate-keys.000.key"
    , "--protocol-version-major", "2"
    , "--protocol-version-minor", "0"
    , "--protocol-version-alt", "0"
    , "--application-name", "cardano-sl"
    , "--software-version-num", "1"
    , "--system-tag", "linux"
    , "--installer-hash", "0"
    ]

  forM_ [1 .. numBftNodes] $ \n ->
    execCli_
      [ "byron", "governance", "create-proposal-vote"
      , "--proposal-filepath", tempAbsPath </> "update-proposal-1"
      , "--testnet-magic", show @Int testnetMagic
      , "--signing-key", tempAbsPath </> "byron/delegate-keys.00" <> show @Int (n - 1) <> ".key"
      , "--vote-yes"
      , "--output-filepath", tempAbsPath </> "update-vote-1.00" <> show @Int (n - 1)
      ]
  -- Generated genesis keys and genesis files
  H.noteEachM_ . H.listDirectory $ tempAbsPath </> "byron"

cardanoTestnetShelleyGenesis
  :: CardanoTestnetOptions
  -> H.Conf
  -> DTC.UTCTime
  -> H.Integration ()
cardanoTestnetShelleyGenesis testnetOptions H.Conf {..} startTime = do

  let numBftNodes = L.length (cardanoBftNodeOptions testnetOptions)
      maxShelleySupply = 1000000000000

  -- Set up our template
  H.createDirectoryIfMissing $ tempAbsPath </> "shelley"

  -- TODO: This is fragile, we should be passing in all necessary
  -- configuration files.
  let sourceAlonzoGenesisSpecFile = base </> "cardano-cli/test/data/golden/alonzo/genesis.alonzo.spec.json"
  alonzoSpecFile <- H.noteTempFile tempAbsPath "shelley/genesis.alonzo.spec.json"
  liftIO $ IO.copyFile sourceAlonzoGenesisSpecFile alonzoSpecFile

  execCli_
    [ "genesis", "create"
    , "--testnet-magic", show @Int testnetMagic
    , "--genesis-dir", tempAbsPath </> "shelley"
    , "--start-time", formatIso8601 startTime
    ]

  -- Then edit the genesis.spec.json ...

  -- We're going to use really quick epochs (300 seconds), by using short slots 0.2s
  -- and K=10, but we'll keep long KES periods so we don't have to bother
  -- cycling KES keys
  H.rewriteJsonFile (tempAbsPath </> "shelley/genesis.spec.json") . J.rewriteObject
    $ HM.insert "activeSlotsCoeff" (J.toJSON @Double (cardanoActiveSlotsCoeff testnetOptions))
    . HM.insert "securityParam" (J.toJSON @Int 10)
    . HM.insert "epochLength" (J.toJSON @Int (cardanoEpochLength testnetOptions))
    . HM.insert "slotLength" (J.toJSON @Double (cardanoSlotLength testnetOptions))
    . HM.insert "maxLovelaceSupply" (J.toJSON @Int maxShelleySupply)
    . flip HM.adjust "protocolParams"
      ( J.rewriteObject ( HM.insert "decentralisationParam" (J.toJSON @Double 0.7)
                        . HM.insert "rho" (J.toJSON @Double 0.1)
                        . HM.insert "tau" (J.toJSON @Double 0.1)
                        )
      )

  -- Now generate for real:
  execCli_
    [ "genesis", "create"
    , "--testnet-magic", show @Int testnetMagic
    , "--genesis-dir", tempAbsPath </> "shelley"
    , "--gen-genesis-keys", show @Int numBftNodes
    , "--start-time", formatIso8601 startTime
    , "--gen-utxo-keys", show @Int (cardanoNumPoolNodes testnetOptions)
    ]

  -- Generated genesis keys and genesis files
  H.noteEachM_ . H.listDirectory $ tempAbsPath </> "shelley"

  H.rewriteJsonFile (tempAbsPath </> "shelley/genesis.json") . J.rewriteObject
    $ flip HM.adjust "protocolParams"
      ( J.rewriteObject
        ( flip HM.adjust "protocolVersion"
          ( J.rewriteObject (HM.insert "major" (J.toJSON @Int 6))
          )
        )
      )
    . HM.insert "updateQuorum" (J.toJSON @Int 2)

  -- Generated shelley/genesis.json
  H.cat $ tempAbsPath </> "shelley/genesis.json"

  -- Generated alonzo/genesis.json
  --TODO: rationalise the naming convention on these genesis json files.
  H.cat $ tempAbsPath </> "shelley/genesis.alonzo.json"

  -- Make the pool operator cold keys
  -- This was done already for the BFT nodes as part of the genesis creation
cardanoTestnetPart5 :: CardanoTestnetOptions -> H.Conf -> H.Integration [PoolNodeKeys]
cardanoTestnetPart5 testnetOptions H.Conf {..} = do

  let numBftNodes = L.length (cardanoBftNodeOptions testnetOptions)
      bftNodesN = [1 .. numBftNodes]
      poolNodesN = [1 .. cardanoNumPoolNodes testnetOptions]
      bftNodeNames = ("node-bft" <>) . show @Int <$> bftNodesN
      poolNodeNames = ("node-pool" <>) . show @Int <$> poolNodesN
      allNodeNames = bftNodeNames <> poolNodeNames

  poolKeys <- forM poolNodesN $ \i -> do
    let node = "node-pool" <> show @Int i

    operatorKeys <- cliNodeKeyGen tempAbsPath
      (node </> "shelley/operator.vkey")
      (node </> "shelley/operator.skey")
      (node </> "shelley/operator.counter")

    poolNodeKeysColdVkey <- H.note $ tempAbsPath </> "node-pool" <> show i <> "/shelley/operator.vkey"
    poolNodeKeysColdSkey <- H.note $ tempAbsPath </> "node-pool" <> show i <> "/shelley/operator.skey"
    poolNodeKeysStakingVkey <- H.note $ tempAbsPath </> node </> "shelley/staking.vkey"
    poolNodeKeysStakingSkey <- H.note $ tempAbsPath </> node </> "shelley/staking.skey"

    nodeVrfKeys <- cliNodeKeyGenVrf tempAbsPath $ KeyNames (node </> "shelley/vrf.vkey") (node </> "shelley/vrf.skey")

    return PoolNodeKeys
      { poolNodeKeysColdVkey
      , poolNodeKeysColdSkey
      , poolNodeKeysVrf = nodeVrfKeys
      , poolNodeKeysOperator = operatorKeys
      , poolNodeKeysStakingVkey
      , poolNodeKeysStakingSkey
      , poolNodeKeysVrfVkey = undefined
      , poolNodeKeysVrfSkey = undefined
      }

  -- Symlink the BFT operator keys from the genesis delegates, for uniformity
  forM_ bftNodesN $ \n -> do
    H.createFileLink (tempAbsPath </> "shelley/delegate-keys/delegate" <> show @Int n <> ".skey") (tempAbsPath </> "node-bft" <> show @Int n </> "shelley/operator.skey")
    H.createFileLink (tempAbsPath </> "shelley/delegate-keys/delegate" <> show @Int n <> ".vkey") (tempAbsPath </> "node-bft" <> show @Int n </> "shelley/operator.vkey")
    H.createFileLink (tempAbsPath </> "shelley/delegate-keys/delegate" <> show @Int n <> ".counter") (tempAbsPath </> "node-bft" <> show @Int n </> "shelley/operator.counter")
    H.createFileLink (tempAbsPath </> "shelley/delegate-keys/delegate" <> show @Int n <> ".vrf.vkey") (tempAbsPath </> "node-bft" <> show @Int n </> "shelley/vrf.vkey")
    H.createFileLink (tempAbsPath </> "shelley/delegate-keys/delegate" <> show @Int n <> ".vrf.skey") (tempAbsPath </> "node-bft" <> show @Int n </> "shelley/vrf.skey")

  -- Make hot keys and for all nodes
  forM_ allNodeNames $ \node -> do
    cliNodeKeyGenKes tempAbsPath $ KeyNames (node </> "shelley/kes.vkey") (node </> "shelley/kes.skey")

    execCli_
      [ "node", "issue-op-cert"
      , "--kes-period", "0"
      , "--kes-verification-key-file", tempAbsPath </> node </> "shelley/kes.vkey"
      , "--cold-signing-key-file", tempAbsPath </> node </> "shelley/operator.skey"
      , "--operational-certificate-issue-counter-file", tempAbsPath </> node </> "shelley/operator.counter"
      , "--out-file", tempAbsPath </> node </> "shelley/node.cert"
      ]

  -- Generated node operator keys (cold, hot) and operational certs
  forM_ allNodeNames $ \node -> H.noteEachM_ . H.listDirectory $ tempAbsPath </> node </> "byron"

  -- Make some payment and stake addresses
  -- user1..n:       will own all the funds in the system, we'll set this up from
  --                 initial utxo the
  -- pool-owner1..n: will be the owner of the pools and we'll use their reward
  --                 account for pool rewards

  return poolKeys

-- End Of Part5 Start Of Part6
cardanoTestnetPart6 :: CardanoTestnetOptions -> H.Conf -> H.Integration [PaymentKeyPair]
cardanoTestnetPart6 testnetOptions H.Conf {..} = do

  let poolNodesN = [1 .. cardanoNumPoolNodes testnetOptions]

-- End Of Part5 Start Of Part6

  let userAddrs = ("user" <>) . show @Int <$> poolNodesN
      poolAddrs = ("pool-owner" <>) . show @Int <$> poolNodesN
      addrs = userAddrs <> poolAddrs

  H.createDirectoryIfMissing $ tempAbsPath </> "addresses"

  wallets <- forM addrs $ \addr -> do
    let paymentSKey = tempAbsPath </> "addresses/" <> addr <> ".skey"
    let paymentVKey = tempAbsPath </> "addresses/" <> addr <> ".vkey"

    -- Payment address keys
    -- TODO !

    cliAddressKeyGen tempAbsPath $ KeyNames ("addresses" </> addr <> ".vkey") ("addresses" </> addr <> ".skey")
    cliAddressKeyGen tempAbsPath $ KeyNames "shelley/utxo-keys/utxo2.vkey" "shelley/utxo-keys/utxo2.skey"
    cliStakeAddressKeyGen tempAbsPath $ KeyNames ("addresses" </> addr <> "-stake.vkey") ("addresses" </> addr <> "-stake.skey")
    cliStakeAddressKeyGen tempAbsPath $ KeyNames "shelley/utxo-keys/utxo-stake.vkey" "shelley/utxo-keys/utxo-stake.skey"
    cliStakeAddressKeyGen tempAbsPath $ KeyNames "shelley/utxo-keys/utxo2-stake.vkey" "shelley/utxo-keys/utxo2-stake.skey"

    -- Payment addresses
    execCli_
      [ "address", "build"
      , "--payment-verification-key-file", tempAbsPath </> "addresses/" <> addr <> ".vkey"
      , "--stake-verification-key-file", tempAbsPath </> "addresses/" <> addr <> "-stake.vkey"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", tempAbsPath </> "addresses/" <> addr <> ".addr"
      ]

    -- Stake addresses
    execCli_
      [ "stake-address", "build"
      , "--stake-verification-key-file", tempAbsPath </> "addresses/" <> addr <> "-stake.vkey"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", tempAbsPath </> "addresses/" <> addr <> "-stake.addr"
      ]

    -- Stake addresses registration certs
    execCli_
      [ "stake-address", "registration-certificate"
      , "--stake-verification-key-file", tempAbsPath </> "addresses/" <> addr <> "-stake.vkey"
      , "--out-file", tempAbsPath </> "addresses/" <> addr <> "-stake.reg.cert"
      ]

    pure $ PaymentKeyPair
      { paymentSKey
      , paymentVKey
      }
  return wallets

cardanoTestnetPart7 :: CardanoTestnetOptions -> H.Conf -> H.Integration ()
cardanoTestnetPart7 testnetOptions H.Conf {..} = do

  let poolNodesN = [1 .. cardanoNumPoolNodes testnetOptions]
      poolNodeNames = ("node-pool" <>) . show @Int <$> poolNodesN
      maxShelleySupply = 1000000000000

  -- user N will delegate to pool N
  forM_ poolNodesN $ \n -> do
    -- Stake address delegation certs
    execCli_
      [ "stake-address", "delegation-certificate"
      , "--stake-verification-key-file", tempAbsPath </> "addresses/user" <> show @Int n <> "-stake.vkey"
      , "--cold-verification-key-file", tempAbsPath </> "node-pool" <> show @Int n </> "shelley/operator.vkey"
      , "--out-file", tempAbsPath </> "addresses/user" <> show @Int n <> "-stake.deleg.cert"
      ]

    H.createFileLink (tempAbsPath </> "addresses/pool-owner" <> show @Int n <> "-stake.vkey") (tempAbsPath </> "node-pool" <> show @Int n </> "owner.vkey")
    H.createFileLink (tempAbsPath </> "addresses/pool-owner" <> show @Int n <> "-stake.skey") (tempAbsPath </> "node-pool" <> show @Int n </> "owner.skey")

  -- Generated payment address keys, stake address keys,
  -- stake address registration certs, and stake address delegation certs
  H.noteEachM_ . H.listDirectory $ tempAbsPath </> "addresses"

  -- Next is to make the stake pool registration cert
  forM_ poolNodeNames $ \node -> do
    H.execCli
      [ "stake-pool", "registration-certificate"
      , "--testnet-magic", show @Int testnetMagic
      , "--pool-pledge", "0", "--pool-cost", "0", "--pool-margin", "0"
      , "--cold-verification-key-file", tempAbsPath </> node </> "shelley/operator.vkey"
      , "--vrf-verification-key-file", tempAbsPath </> node </> "shelley/vrf.vkey"
      , "--reward-account-verification-key-file", tempAbsPath </> node </> "owner.vkey"
      , "--pool-owner-stake-verification-key-file", tempAbsPath </> node </> "owner.vkey"
      , "--out-file", tempAbsPath </> node </> "registration.cert"
      ]

  -- Generated stake pool registration certs
  forM_ poolNodeNames $ \node -> H.assertIO . IO.doesFileExist $ tempAbsPath </> node </> "registration.cert"

  -- Now we'll construct one whopper of a transaction that does everything
  -- just to show off that we can, and to make the script shorter

  do
    -- We'll transfer all the funds to the user1, which delegates to pool1
    -- We'll register certs to:
    --  1. register the pool-owner1 stake address
    --  2. register the stake pool 1
    --  3. register the user1 stake address
    --  4. delegate from the user1 stake address to the stake pool
    txIn <- H.noteShow . S.strip =<< H.execCli
      [ "genesis", "initial-txin"
      , "--testnet-magic", show @Int testnetMagic
      , "--verification-key-file", tempAbsPath </> "shelley/utxo-keys/utxo1.vkey"
      ]

    H.note_ txIn

    user1Addr <- H.readFile $ tempAbsPath </> "addresses/user1.addr"

    execCli_
      [ "transaction", "build-raw"
      , "--invalid-hereafter", "1000"
      , "--fee", "0"
      , "--tx-in", txIn
      , "--tx-out",  user1Addr <> "+" <> show @Int maxShelleySupply
      , "--certificate-file", tempAbsPath </> "addresses/pool-owner1-stake.reg.cert"
      , "--certificate-file", tempAbsPath </> "node-pool1/registration.cert"
      , "--certificate-file", tempAbsPath </> "addresses/user1-stake.reg.cert"
      , "--certificate-file", tempAbsPath </> "addresses/user1-stake.deleg.cert"
      , "--out-file", tempAbsPath </> "tx1.txbody"
      ]
  -- TODO: this will become the transaction to register the pool, etc.
  -- We'll need to pick the tx-in from the actual UTxO since it contains the txid,
  -- we'll have to query this via cardano-cli query utxo.

  {-  cardano-cli transaction build-raw \
          --invalid-hereafter 1000000 --fee 0 \
          --tx-in 67209bfcdf78f8cd86f649da75053a80fb9bb3fad68465554f9301c31b496c65#0 \
          --tx-out $(cat example/addresses/user1.addr)+450000000 \
          --certificate-file example/addresses/pool-owner1-stake.reg.cert \
          --certificate-file example/node-pool1/registration.cert \
          --certificate-file example/addresses/user1-stake.reg.cert \
          --certificate-file example/addresses/user1-stake.deleg.cert \
          --out-file example/register-pool.txbody
  -}

  {-  cardano-cli address convert \
          --byron-key-file example/byron/payment-keys.000.key \
          --signing-key-file example/byron/payment-keys.000-converted.key
  -}

  {-  cardano-cli transaction sign \
          --tx-body-file example/register-pool.txbody \
          --testnet-magic 42 \
          --signing-key-file example/byron/payment-keys.000-converted.key \
          --signing-key-file example/shelley/utxo-keys/utxo1.skey \
          --signing-key-file example/addresses/user1-stake.skey \
          --signing-key-file example/node-pool1/owner.skey \
          --signing-key-file example/node-pool1/shelley/operator.skey \
          --out-file example/register-pool.tx
  -}

  {-  cardano-cli transaction submit \
          --tx-file example/register-pool.tx --testnet-magic 42
  -}

  -- So we'll need to sign this with a bunch of keys:
  -- 1. the initial utxo spending key, for the funds
  -- 2. the user1 stake address key, due to the delegation cert
  -- 3. the pool1 owner key, due to the pool registration cert
  -- 3. the pool1 operator key, due to the pool registration cert
  execCli_
    [ "transaction", "sign"
    , "--signing-key-file", tempAbsPath </> "shelley/utxo-keys/utxo1.skey"
    , "--signing-key-file", tempAbsPath </> "addresses/user1-stake.skey"
    , "--signing-key-file", tempAbsPath </> "node-pool1/owner.skey"
    , "--signing-key-file", tempAbsPath </> "node-pool1/shelley/operator.skey"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-body-file", tempAbsPath </> "tx1.txbody"
    , "--out-file", tempAbsPath </> "tx1.tx"
    ]

  -- Generated a signed 'do it all' transaction:
  H.assertIO . IO.doesFileExist $ tempAbsPath </> "tx1.tx"

  -- Add Byron, Shelley and Alonzo genesis hashes to node configuration
  byronGenesisHash <- getByronGenesisHash $ tempAbsPath </> "byron/genesis.json"
  shelleyGenesisHash <- getShelleyGenesisHash $ tempAbsPath </> "shelley/genesis.json"
  alonzoGenesisHash <- getShelleyGenesisHash $ tempAbsPath </> "shelley/genesis.alonzo.json"
  H.rewriteYamlFile (tempAbsPath </> "configuration.yaml") . J.rewriteObject
    $ HM.insert "ByronGenesisHash" byronGenesisHash
    . HM.insert "ShelleyGenesisHash" shelleyGenesisHash
    . HM.insert "AlonzoGenesisHash" alonzoGenesisHash

cardanoTestnetLaunchCluster
  :: CardanoTestnetOptions
  -> H.Conf
  -> [PoolNodeKeys]
  -> [PaymentKeyPair]
  -> H.Integration TestnetRuntime
cardanoTestnetLaunchCluster testnetOptions H.Conf {..} poolKeys wallets = do

  let numBftNodes = L.length (cardanoBftNodeOptions testnetOptions)
      bftNodesN = [1 .. numBftNodes]
      poolNodesN = [1 .. cardanoNumPoolNodes testnetOptions]
      bftNodeNames = ("node-bft" <>) . show @Int <$> bftNodesN
      poolNodeNames = ("node-pool" <>) . show @Int <$> poolNodesN
      allNodeNames = bftNodeNames <> poolNodeNames

  configurationFile <- H.noteShow $ tempAbsPath </> "configuration.yaml"
  --------------------------------
  -- Launch cluster of three nodes

  let bftNodeNameAndOpts = L.zip bftNodeNames (cardanoBftNodeOptions testnetOptions)
  bftNodes <- forM bftNodeNameAndOpts $ \(node, nodeOpts) -> do
    startNode tempBaseAbsPath tempAbsPath logDir socketDir node
      ([ "run"
        , "--config",  tempAbsPath </> "configuration.yaml"
        , "--topology",  tempAbsPath </> node </> "topology.json"
        , "--database-path", tempAbsPath </> node </> "db"
        , "--shelley-kes-key", tempAbsPath </> node </> "shelley/kes.skey"
        , "--shelley-vrf-key", tempAbsPath </> node </> "shelley/vrf.skey"
        , "--shelley-operational-certificate", tempAbsPath </> node </> "shelley/node.cert"
        , "--delegation-certificate",  tempAbsPath </> node </> "byron/delegate.cert"
        , "--signing-key", tempAbsPath </> node </> "byron/delegate.key"
        ] <> extraNodeCliArgs nodeOpts)

  H.threadDelay 100000

  poolNodes <- forM (L.zip poolNodeNames poolKeys) $ \(node, key) -> do
    runtime <- startNode tempBaseAbsPath tempAbsPath logDir socketDir node
        [ "run"
        , "--config", tempAbsPath </> "configuration.yaml"
        , "--topology", tempAbsPath </> node </> "topology.json"
        , "--database-path", tempAbsPath </> node </> "db"
        , "--shelley-kes-key", tempAbsPath </> node </> "shelley/kes.skey"
        , "--shelley-vrf-key", tempAbsPath </> node </> "shelley/vrf.skey"
        , "--shelley-operational-certificate", tempAbsPath </> node </> "shelley/node.cert"
        , "--host-addr", ifaceAddress
        ]
    return $ PoolNode runtime key

  now <- H.noteShowIO DTC.getCurrentTime
  deadline <- H.noteShow $ DTC.addUTCTime 90 now

  forM_ allNodeNames $ \node -> do
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir </> node)
    _spocketSystemNameFile <- H.noteShow $ IO.sprocketSystemName sprocket
    H.byDeadlineM 10 deadline "Failed to connect to node socket" $ H.assertM $ H.doesSprocketExist sprocket

  forM_ allNodeNames $ \node -> do
    nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
    H.assertChainExtended deadline (cardanoNodeLoggingFormat testnetOptions) nodeStdoutFile

  H.noteShowIO_ DTC.getCurrentTime

  return TestnetRuntime
    { configurationFile
    , shelleyGenesisFile = tempAbsPath </> "shelley/genesis.json"
    , testnetMagic
    , bftNodes
    , poolNodes
    , wallets
    , delegators = [] -- TODO this should be populated
    }

-- * Generate hashes for genesis.json files

getByronGenesisHash :: (H.MonadTest m, MonadIO m) => FilePath -> m J.Value
getByronGenesisHash path = do
  e <- runExceptT $ readGenesisData path
  (_, genesisHash) <- H.leftFail e
  let genesisHash' = J.toJSON $ unGenesisHash genesisHash
  pure genesisHash'

getShelleyGenesisHash :: (H.MonadTest m, MonadIO m) => FilePath -> m J.Value
getShelleyGenesisHash path = do
  content <- liftIO $ BS.readFile path
  let genesisHash = Cardano.Crypto.Hash.Class.hashWith id content :: Cardano.Crypto.Hash.Class.Hash Cardano.Crypto.Hash.Blake2b.Blake2b_256 BS.ByteString
  pure $ J.toJSON genesisHash
