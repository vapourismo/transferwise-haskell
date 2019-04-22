{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}

module Network.Transferwise
    ( -- * API token
      ApiToken (..)
    , HasApiToken
    , withApiToken

      -- * Servant
    , API.transferwiseApiBaseUrl
    , API.transferwiseSandboxApiBaseUrl

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
    , Account (..)
    , accounts
    , Transaction (..)
    , transactionsFor

      -- * Quotes
    , TempQuote (..)
    , tempQuote
    , Quote (..)
    , quote

      -- * Auxiliary types
    , ProfileId
    , AddressId
    , AccountId
    , UserId
    , ProfileType (..)
    , Balance (..)
    , Conversion (..)
    , QuoteType (..)
    , Grouping (..)
    , TransactionType (..)
    , TransactionDetails (..)
    )
where

import Control.Monad.Except (catchError, throwError)

import qualified Data.Aeson      as Aeson
import           Data.Text       (Text)
import           Data.Time.Clock (UTCTime)

import Money (someDenseAmount, someDenseCurrency)

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
    => Text -- ^ Pay currency
    -> Text -- ^ Receive currency
    -> ClientM ExchangeRate
exchangeRate payCcy recvCcy =
    API.getExchangeRate ?apiToken payCcy recvCcy Nothing >>= \case
        [rate] -> pure rate
        rates  -> fail ("Received to unexpected number of exchange rates: " <> show (length rates))

-- | Retrieve one exchange rate at a given time.
exchangeRateAt
    :: HasApiToken
    => Text     -- ^ Pay currency
    -> Text     -- ^ Receive currency
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
    => Text           -- ^ Pay currency
    -> Text           -- ^ Receive currency
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

-- | Retrieve all transactions within the given time frame for a specific currency.
transactionsFor
    :: HasApiToken
    => AccountId -- ^ Borderless account ID
    -> Text      -- ^ Currency
    -> UTCTime   -- ^ Interval start
    -> UTCTime   -- ^ Interval end
    -> ClientM [Transaction]
transactionsFor accountId currency start end =
    statementTransactions <$> API.getStatement ?apiToken accountId currency start end

----------------------------------------------------------------------------------------------------
-- Quotes

-- | Get a temporary quote.
tempQuote :: HasApiToken => Conversion -> ClientM TempQuote
tempQuote = ignoreWarning . \case
    ConvertFrom pay recvCcy ->
        API.createTempQuote
            ?apiToken
            (someDenseCurrency pay)
            (Just (Amount (someDenseAmount pay)))
            recvCcy
            Nothing
            Fixed

    ConvertTo payCcy receive ->
        API.createTempQuote
            ?apiToken
            payCcy
            Nothing
            (someDenseCurrency receive)
            (Just (Amount (someDenseAmount receive)))
            Fixed
    where
        ignoreWarning :: ClientM TempQuote -> ClientM TempQuote
        ignoreWarning action = catchError action $ \case
            FailureResponse _ response | Just tempQuote <- Aeson.decode (responseBody response) ->
                pure tempQuote

            other -> throwError other

-- | Create a quote.
quote :: HasApiToken => ProfileId -> QuoteType -> Conversion -> ClientM Quote
quote profileId quoteType conversion =
    API.createQuote ?apiToken (CreateQuote profileId quoteType conversion)
