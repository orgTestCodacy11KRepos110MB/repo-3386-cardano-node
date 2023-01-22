{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Convenience query functions
--
module Cardano.Api.Convenience.Query (
    QueryConvenienceError(..),
    determineEra,
    determineEra_,
    -- * Simplest query related
    executeQueryCardanoMode,

    queryStateForBalancedTx,
    renderQueryConvenienceError,
  ) where

import           Prelude

import qualified Control.Monad.Oops as OO
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (ExceptT (..), except, runExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistMaybe, left)
import           Data.Bifunctor (first)
import           Data.Function ((&))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)

import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch (..))

import           Cardano.Api.Certificate
import           Cardano.Api.Convenience.Constraints
import           Cardano.Api.Environment
import           Cardano.Api.Eras
import           Cardano.Api.IPC
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query
import           Cardano.Api.TxBody
import           Cardano.Api.Utils

data QueryConvenienceError
  = AcqFailure AcquiringFailure
  | SockErr EnvSocketError
  | QueryEraMismatch EraMismatch
  | ByronEraNotSupported
  | EraConsensusModeMismatch !AnyConsensusMode !AnyCardanoEra

renderQueryConvenienceError :: QueryConvenienceError -> Text
renderQueryConvenienceError (AcqFailure e) =
  "Acquiring failure: " <> textShow e
renderQueryConvenienceError (SockErr e) =
  renderEnvSocketError e
renderQueryConvenienceError (QueryEraMismatch (EraMismatch ledgerEraName' otherEraName')) =
  "The era of the node and the tx do not match. " <>
  "The node is running in the " <> ledgerEraName' <>
  " era, but the transaction is for the " <> otherEraName' <> " era."
renderQueryConvenienceError ByronEraNotSupported =
  "Byron era not supported"
renderQueryConvenienceError (EraConsensusModeMismatch cMode anyCEra) =
  "Consensus mode and era mismatch. Consensus mode: " <> textShow cMode <>
  " Era: " <> textShow anyCEra

-- | A convenience function to query the relevant information, from
-- the local node, for Cardano.Api.Convenience.Construction.constructBalancedTx
queryStateForBalancedTx
  :: CardanoEra era
  -> NetworkId
  -> [TxIn]
  -> IO (Either QueryConvenienceError (UTxO era, ProtocolParameters, EraHistory CardanoMode, SystemStart, Set PoolId))
queryStateForBalancedTx era networkId allTxIns = runExceptT $ OO.runOopsInExceptT @QueryConvenienceError $ do
  SocketPath sockPath <- lift readEnvSocketPath & OO.onLeft (OO.throw . SockErr)

  let cModeParams = CardanoModeParams $ EpochSlots 21600

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams networkId sockPath

  qSbe <- getSbe (cardanoEraStyle era) & OO.hoistEither

  qeInMode <- toEraInMode era CardanoMode
    & OO.hoistMaybe (EraConsensusModeMismatch (AnyConsensusMode CardanoMode) (getIsCardanoEraConstraint era $ AnyCardanoEra era))

  -- Queries
  let utxoQuery = QueryInEra qeInMode $ QueryInShelleyBasedEra qSbe
                    $ QueryUTxO (QueryUTxOByTxIn (Set.fromList allTxIns))
      pparamsQuery = QueryInEra qeInMode
                        $ QueryInShelleyBasedEra qSbe QueryProtocolParameters
      eraHistoryQuery = QueryEraHistory CardanoModeIsMultiEra
      systemStartQuery = QuerySystemStart
      stakePoolsQuery = QueryInEra qeInMode . QueryInShelleyBasedEra qSbe $ QueryStakePools

  -- Query execution
  utxo <- queryNodeLocalState_ localNodeConnInfo Nothing utxoQuery
      & OO.onLeft @EraMismatch (OO.throw . QueryEraMismatch)
      & OO.catch @AcquireFailure (OO.throw . AcqFailure . toAcquiringFailure)
  pparams <- queryNodeLocalState_ localNodeConnInfo Nothing pparamsQuery
      & OO.onLeft @EraMismatch (OO.throw . QueryEraMismatch)
      & OO.catch @AcquireFailure (OO.throw . AcqFailure . toAcquiringFailure)
  eraHistory <- queryNodeLocalState_ localNodeConnInfo Nothing eraHistoryQuery
      & OO.catch @AcquireFailure (OO.throw . AcqFailure . toAcquiringFailure)
  systemStart <- queryNodeLocalState_ localNodeConnInfo Nothing systemStartQuery
      & OO.catch @AcquireFailure (OO.throw . AcqFailure . toAcquiringFailure)
  stakePools <- queryNodeLocalState_ localNodeConnInfo Nothing stakePoolsQuery
      & OO.onLeft @EraMismatch (OO.throw . QueryEraMismatch)
      & OO.catch @AcquireFailure (OO.throw . AcqFailure . toAcquiringFailure)

  return (utxo, pparams, eraHistory, systemStart, stakePools)

-- | Query the node to determine which era it is in.
determineEra
  :: ConsensusModeParams mode
  -> LocalNodeConnectInfo mode
  -> IO (Either AcquiringFailure AnyCardanoEra)
determineEra cModeParams localNodeConnInfo =
  case consensusModeOnly cModeParams of
    ByronMode -> return . Right $ AnyCardanoEra ByronEra
    ShelleyMode -> return . Right $ AnyCardanoEra ShelleyEra
    CardanoMode ->
      queryNodeLocalState localNodeConnInfo Nothing
         $ QueryCurrentEra CardanoModeIsMultiEra

-- | Query the node to determine which era it is in.
determineEra_
  :: forall e mode. ()
  => e `OO.CouldBe` AcquireFailure
  => ConsensusModeParams mode
  -> LocalNodeConnectInfo mode
  -> ExceptT (OO.Variant e) IO AnyCardanoEra
determineEra_ cModeParams localNodeConnInfo =
  case consensusModeOnly cModeParams of
    ByronMode -> pure $ AnyCardanoEra ByronEra
    ShelleyMode -> pure $ AnyCardanoEra ShelleyEra
    CardanoMode ->
      queryNodeLocalState_ localNodeConnInfo Nothing
        $ QueryCurrentEra CardanoModeIsMultiEra

getSbe :: CardanoEraStyle era -> Either QueryConvenienceError (ShelleyBasedEra era)
getSbe LegacyByronEra = Left ByronEraNotSupported
getSbe (ShelleyBasedEra sbe) = return sbe

-- | Execute a query against the local node. The local
-- node must be in CardanoMode.
executeQueryCardanoMode
  :: CardanoEra era
  -> NetworkId
  -> QueryInMode CardanoMode (Either EraMismatch result)
  -> IO (Either QueryConvenienceError result)
executeQueryCardanoMode era nid q = runExceptT $ do
  SocketPath sockPath <- firstExceptT SockErr . ExceptT $ readEnvSocketPath

  let localConnectInfo =
        LocalNodeConnectInfo
          { localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
          , localNodeNetworkId = nid
          , localNodeSocketPath = sockPath
          }

  ExceptT $ executeQueryAnyMode era localConnectInfo q

-- | Execute a query against the local node in any mode.
executeQueryAnyMode
  :: forall result era mode. CardanoEra era
  -> LocalNodeConnectInfo mode
  -> QueryInMode mode (Either EraMismatch result)
  -> IO (Either QueryConvenienceError result)
executeQueryAnyMode era localNodeConnInfo q = runExceptT $ do
  let cMode = consensusModeOnly $ localConsensusModeParams localNodeConnInfo

  eraInMode <- toEraInMode era cMode
    & hoistMaybe (EraConsensusModeMismatch (AnyConsensusMode CardanoMode) (getIsCardanoEraConstraint era $ AnyCardanoEra era))

  case eraInMode of
    ByronEraInByronMode -> left ByronEraNotSupported
    _ -> execQuery
  where
    execQuery :: ExceptT QueryConvenienceError IO result
    execQuery = do
      r <- OO.runOopsInExceptT @QueryConvenienceError $ do
        queryNodeLocalState_ localNodeConnInfo Nothing q
          & OO.catch @AcquireFailure (OO.throw . AcqFailure . toAcquiringFailure)
      except $ first QueryEraMismatch r
