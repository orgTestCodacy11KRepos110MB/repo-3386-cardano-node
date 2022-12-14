{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.IPC.Monad
  ( LocalStateQueryExpr
  , executeLocalStateQueryExpr_
  , queryExpr
  , queryExpr_
  , determineEraExpr_
  , NodeToClientVersionOf (..)
  , getNtcVersion_
  , maybeQueryExpr_
  ) where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Control.Monad.Oops as OO
import           Control.Monad.Reader
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Except
import qualified Data.Variant as DV

import           Cardano.Api.Block
import           Cardano.Api.Eras
import           Cardano.Api.IPC
import           Cardano.Api.IPC.Version
import           Cardano.Api.Modes
import           Cardano.Ledger.Shelley.Scripts ()

import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Net.Query

{- HLINT ignore "Use const" -}
{- HLINT ignore "Use let" -}

-- | Monadic type for constructing local state query expressions.
--
-- Use 'queryExpr' in a do block to construct queries of this type and convert
-- the expression to a 'Net.Query.LocalStateQueryClient' with 'setupLocalStateQueryExpr_'.
--
-- Some consideration was made to use Applicative instead of Monad as the abstraction in
-- order to support pipelining, but we actually have a fair amount of code where the next
-- query depends on the result of the former and therefore actually need Monad.
--
-- In order to make pipelining still possible we can explore the use of Selective Functors
-- which would allow us to straddle both worlds.
newtype LocalStateQueryExpr block point query r m a = LocalStateQueryExpr
  { runLocalStateQueryExpr :: ReaderT NodeToClientVersion (ContT (Net.Query.ClientStAcquired block point query m r) m) a
  } deriving (Functor, Applicative, Monad, MonadReader NodeToClientVersion, MonadIO)

-- | Execute a local state query expression.
executeLocalStateQueryExpr_
  :: forall e mode a . ()
  => e `OO.CouldBe` AcquireFailure
  => LocalNodeConnectInfo mode
  -> Maybe ChainPoint
  -> ExceptT (OO.Variant e) (LocalStateQueryExpr (BlockInMode mode) ChainPoint (QueryInMode mode) () IO) a
  -> ExceptT (OO.Variant e) IO a
executeLocalStateQueryExpr_ connectInfo mpoint f = do
  tmvResultLocalState <- liftIO (newEmptyTMVarIO @(Either (OO.Variant e) a))
  let waitResult = readTMVar tmvResultLocalState

  liftIO $ connectToLocalNodeWithVersion
    connectInfo
    (\ntcVersion ->
      LocalNodeClientProtocols
      { localChainSyncClient    = NoLocalChainSyncClient
      , localStateQueryClient   = Just $ setupLocalStateQueryExpr_ waitResult mpoint tmvResultLocalState ntcVersion f
      , localTxSubmissionClient = Nothing
      , localTxMonitoringClient = Nothing
      }
    )

  ExceptT . return =<< liftIO (atomically waitResult)

-- | Use 'queryExpr' in a do block to construct monadic local state queries.
setupLocalStateQueryExpr_ :: ()
  => e `OO.CouldBe` AcquireFailure
  => STM x
     -- ^ An STM expression that only returns when all protocols are complete.
     -- Protocols must wait until 'waitDone' returns because premature exit will
     -- cause other incomplete protocols to abort which may lead to deadlock.
  -> Maybe ChainPoint
  -> TMVar (Either (OO.Variant e) a)
  -> NodeToClientVersion
  -> ExceptT (OO.Variant e) (LocalStateQueryExpr (BlockInMode mode) ChainPoint (QueryInMode mode) () IO) a
  -> Net.Query.LocalStateQueryClient (BlockInMode mode) ChainPoint (QueryInMode mode) IO ()
setupLocalStateQueryExpr_ waitDone mPointVar' resultVar' ntcVersion f =
  LocalStateQueryClient . pure . Net.Query.SendMsgAcquire mPointVar' $
    Net.Query.ClientStAcquiring
    { Net.Query.recvMsgAcquired = runContT (runReaderT (runLocalStateQueryExpr (runExceptT f)) ntcVersion) $ \result -> do
        atomically $ putTMVar resultVar' result
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgRelease $ pure $ Net.Query.SendMsgDone ()

    , Net.Query.recvMsgFailure = \acquireFailure -> do
        atomically $ putTMVar resultVar' (Left (DV.throw acquireFailure))
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgDone ()
    }

-- | Get the node server's Node-to-Client version.
getNtcVersion :: LocalStateQueryExpr block point (QueryInMode mode) r IO NodeToClientVersion
getNtcVersion = LocalStateQueryExpr ask

-- | Use 'queryExpr' in a do block to construct monadic local state queries.
queryExpr :: QueryInMode mode a -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError a)
queryExpr q = do
  let minNtcVersion = nodeToClientVersionOf q
  ntcVersion <- getNtcVersion
  if ntcVersion >= minNtcVersion
    then
      fmap Right . LocalStateQueryExpr . ReaderT $ \_ -> ContT $ \f -> pure $
        Net.Query.SendMsgQuery q $
          Net.Query.ClientStQuerying
          { Net.Query.recvMsgResult = f
          }
    else pure (Left (UnsupportedNtcVersionError minNtcVersion ntcVersion))

-- | Get the node server's Node-to-Client version.
getNtcVersion_ :: ExceptT (OO.Variant e) (LocalStateQueryExpr block point (QueryInMode mode) r IO) NodeToClientVersion
getNtcVersion_ = ExceptT $ LocalStateQueryExpr $ do
  v <- ask
  pure $ Right v

-- | Lift a query value into a monadic query expression.
-- Use 'queryExpr' in a do block to construct monadic local state queries.
queryExpr_ :: ()
  => QueryInMode mode a
  -> ExceptT (OO.Variant e) (LocalStateQueryExpr block point (QueryInMode mode) r IO) a
queryExpr_ q = lift $ LocalStateQueryExpr $ ReaderT $ \_ -> ContT $ \f -> pure $
  Net.Query.SendMsgQuery q $
    Net.Query.ClientStQuerying
    { Net.Query.recvMsgResult = f
    }

-- | Lift a query value into a monadic query expression returning Maybe of a result.
-- This is the same as 'queryExpr' except if the query is not supported by the server, will return Nothing instead
-- of throwing an error.
maybeQueryExpr_ :: ()
  => QueryInMode mode a
  -> ExceptT (OO.Variant e) (LocalStateQueryExpr block point (QueryInMode mode) r IO) (Maybe a)
maybeQueryExpr_ q = fmap Just (queryExpr_ q)

-- | A monadic expresion that determines what era the node is in.
determineEraExpr_ :: ()
  => ConsensusModeParams mode
  -> ExceptT (OO.Variant e) (LocalStateQueryExpr block point (QueryInMode mode) r IO) AnyCardanoEra
determineEraExpr_ cModeParams =
  case consensusModeOnly cModeParams of
    ByronMode -> return $ AnyCardanoEra ByronEra
    ShelleyMode -> return $ AnyCardanoEra ShelleyEra
    CardanoMode -> queryExpr_ $ QueryCurrentEra CardanoModeIsMultiEra
