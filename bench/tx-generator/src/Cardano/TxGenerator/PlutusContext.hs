{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module  Cardano.TxGenerator.PlutusContext
        ( PlutusAutoBudgetSummary(..)
        , PlutusAutoLimitingFactor(..)
        , plutusAutoBudgetMaxOut
        , readScriptData
        , scriptDataModifyNumber
        )
        where

import           GHC.Generics (Generic)

import           Control.Monad.Trans.Except.Extra
import           Data.Aeson as Aeson

import           Cardano.Api
import           Cardano.Api.Shelley (ProtocolParameters)

import           Cardano.TxGenerator.Setup.Plutus (preExecutePlutusScript)
import           Cardano.TxGenerator.Types


data PlutusAutoBudgetSummary =
  PlutusAutoBudgetSummary
  { budgetPerTx         :: !ExecutionUnits
  , budgetPerTxInput    :: !ExecutionUnits
  , scriptId            :: !FilePath
  , loopCounter         :: !Integer
  , loopLimitingFactors :: ![PlutusAutoLimitingFactor]
  , budgetUsed          :: !ExecutionUnits
  , scriptDatum         :: !ScriptData
  , scriptRedeemer      :: !ScriptData
  }
  deriving (Generic, ToJSON)

data PlutusAutoLimitingFactor
  = ExceededMemoryLimit
  | ExceededExecutionLimit
  deriving (Generic, Eq, Show, ToJSON)

instance ToJSON ScriptData where
  toJSON = scriptDataToJson ScriptDataJsonDetailedSchema


-- | load serialized ScriptData, filling in an empty value if no .json file is given
readScriptData :: FilePath -> IO (Either TxGenError ScriptData)
readScriptData ""
  = pure $ Right $ ScriptDataNumber 0             -- TODO: make sure this is an adequate empty value
readScriptData jsonFilePath
  = runExceptT $ do
    sData :: Aeson.Value <-
      firstExceptT TxGenError . hoistEither =<<
        handleIOExceptT (TxGenError . show) (eitherDecodeFileStrict' jsonFilePath)
    firstExceptT ApiError . hoistEither $
       scriptDataFromJson ScriptDataJsonDetailedSchema sData

-- | Use a binary search to find a loop counter that maxes out the available script execution units.
--   plutusAutoBudgetMaxOut makes two assumptions about the loop / PlutusAuto script:
--   1. The redeemer passed in is a valid one, and encodes i.a. the loop's
--      termination value when counting down.
--   2. In the redeemer's argument structure, this value is the first numerical value
--      that's encountered during traversal.
plutusAutoBudgetMaxOut :: ProtocolParameters -> ScriptInAnyLang -> PlutusAutoBudget -> Either TxGenError (PlutusAutoBudget, Integer, [PlutusAutoLimitingFactor])
plutusAutoBudgetMaxOut protocolParams script pab@PlutusAutoBudget{..}
  = do
    (n, limitFactors) <- binarySearch isInLimits 0 searchUpperBound
    pure (pab {autoBudgetRedeemer = toLoopArgument n}, n, limitFactors)
  where
    -- The highest loop counter that is tried - this is about 10 times the current mainnet limit.
    searchUpperBound = 20000

    toLoopArgument n = scriptDataModifyNumber (+ n) autoBudgetRedeemer

    -- the execution is considered within limits when there's no limiting factor, i.e. the list is empty
    isInLimits :: Integer -> Either TxGenError [PlutusAutoLimitingFactor]
    isInLimits n = do
      used <- preExecutePlutusScript protocolParams script autoBudgetDatum (toLoopArgument n)
      pure $   [ExceededExecutionLimit | executionSteps used > executionSteps autoBudgetUnits]
            ++ [ExceededMemoryLimit    | executionMemory used > executionMemory autoBudgetUnits]

-- modifies the first ScriptDataNumber encountered during traversal to the value provided
scriptDataModifyNumber :: (Integer -> Integer) -> ScriptData -> ScriptData
scriptDataModifyNumber f
  = go
  where
    go = \case
      ScriptDataNumber i -> ScriptDataNumber (f i)
      ScriptDataConstructor int list -> ScriptDataConstructor int (goList list)
      ScriptDataList list -> ScriptDataList (goList list)
      ScriptDataMap m ->
        let {(ks, vs) = unzip m; vs' = goList vs}
        in ScriptDataMap (zip ks vs')
      other -> other
    goList [] = []
    goList (x:xs) =
      let x' = go x in if x' == x then x : goList xs else x' : xs

binarySearch :: (Integral n, Show n) => (n -> Either TxGenError [a]) -> n -> n -> Either TxGenError (n, [a])
binarySearch f a_ b_ = do
  l <- withinLimits <$> f a_
  h <- withinLimits <$> f b_
  if l && not h
    then go [] a_ b_
    else Left $ TxGenError $ "binarySearch: bad initial bounds: " ++ show (a_,l,b_,h)
  where
    withinLimits = null
    go limitingFactors a b
      | a + 1 == b = Right (a, limitingFactors)
      | otherwise = do
          let m = (a + b) `div` 2
          limitingFactors' <- f m
          if withinLimits limitingFactors'
            then go limitingFactors m b
            else go limitingFactors' a m
