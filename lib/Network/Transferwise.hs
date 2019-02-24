{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}

module Network.Transferwise
    ( -- * API token
      ApiToken (..)
    , HasApiToken
    , withApiToken

      -- * Exchange rates
    , ExchangeRate (..)
    , exchangeRates
    , exchangeRate
    , exchangeRateAt
    , exchangeRatesDuring

      -- * Profiles
    , Profile (..)
    , profiles

      -- * Accounts
    , accounts

      -- * Auxiliary types
    , Currency (..)
    , ProfileId
    , ProfileType (..)
    , AddressId
    , AccountId
    , Balance (..)
    )
where

import Data.Time.Clock (UTCTime)

import Servant.Client

import qualified Network.Transferwise.API   as API
import           Network.Transferwise.Types

----------------------------------------------------------------------------------------------------
-- API token

-- | Constraint that ensures the existence of an API token
type HasApiToken = ?apiToken :: ApiToken

-- | Inject the API token into an operation that needs it.
withApiToken
    :: ApiToken           -- ^ API token
    -> (HasApiToken => a) -- ^ Operation to perform with the API token
    -> a
withApiToken apiToken action = let ?apiToken = apiToken in action

----------------------------------------------------------------------------------------------------
-- Exchange rates

-- | Retrieve all exchange rates.
exchangeRates :: HasApiToken => ClientM [ExchangeRate]
exchangeRates = API.getExchangeRates ?apiToken

-- | Retrieve one exchange rate.
exchangeRate
    :: HasApiToken
    => Currency -- ^ Pay currency
    -> Currency -- ^ Receive currency
    -> ClientM ExchangeRate
exchangeRate payCcy recvCcy =
    API.getExchangeRate ?apiToken payCcy recvCcy Nothing >>= \case
        [rate] -> pure rate
        rates  -> fail ("Received to unexpected number of exchange rates: " <> show (length rates))

-- | Retrieve one exchange rate at a given time.
exchangeRateAt
    :: HasApiToken
    => Currency -- ^ Pay currency
    -> Currency -- ^ Receive currency
    -> UTCTime  -- ^ Approximate point in time
    -> ClientM ExchangeRate
exchangeRateAt payCcy recvCcy time =
    API.getExchangeRate ?apiToken payCcy recvCcy (Just time) >>= \case
        [rate] -> pure rate
        rates  -> fail ("Received to unexpected number of exchange rates: " <> show (length rates))

-- | Retrieve exchanges during a given time period. The grouping is optional, providing 'Nothing'
-- will let the Transferwise API choose the grouping.
exchangeRatesDuring
    :: HasApiToken
    => Currency       -- ^ Pay currency
    -> Currency       -- ^ Receive currency
    -> UTCTime        -- ^ Period begin
    -> UTCTime        -- ^ Period end
    -> Maybe Grouping -- ^ Grouping ('Nothing' does not mean no grouping)
    -> ClientM [ExchangeRate]
exchangeRatesDuring = API.getExchangeRatesDuring ?apiToken

----------------------------------------------------------------------------------------------------
-- Profiles

-- | List the profiles that are associated with the API token.
profiles :: HasApiToken => ClientM [Profile]
profiles = API.getProfiles ?apiToken

----------------------------------------------------------------------------------------------------
-- Accounts

-- | Retrieve a list of accounts associated with the given profile.
accounts :: HasApiToken => ProfileId -> ClientM [Account]
accounts = API.getAccounts ?apiToken
