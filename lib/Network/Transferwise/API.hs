{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Network.Transferwise.API
    ( -- * Servant basics
      transferwiseApiBaseUrl
    , transferwiseSandboxApiBaseUrl

      -- * Exchange rates
    , getExchangeRates
    , getExchangeRate
    , getExchangeRateOverTime

      -- * Profiles
    , getProfiles

      -- * Quotes
    , createQuote
    , createTemporaryQuote

      -- * Borderless Accounts
    , getAccounts
    )
where

import Data.Proxy (Proxy (..))
import Data.Time  (UTCTime)

import Servant.API
import Servant.Client

import Network.Transferwise.Types

----------------------------------------------------------------------------------------------------
-- API base URL

transferwiseApiBaseUrl :: BaseUrl
transferwiseApiBaseUrl = BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost   = "api.transferwise.com"
    , baseUrlPort   = 443
    , baseUrlPath   = ""
    }

transferwiseSandboxApiBaseUrl :: BaseUrl
transferwiseSandboxApiBaseUrl = BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost   = "api.sandbox.transferwise.tech"
    , baseUrlPort   = 443
    , baseUrlPath   = ""
    }

----------------------------------------------------------------------------------------------------
-- Utilities

type AuthorizationHeader = Header' '[Strict, Required] "Authorization" ApiToken

----------------------------------------------------------------------------------------------------
-- Exchange rate

type GetExchangeRatesApi =
    AuthorizationHeader
    :> "rates"
    :> Get '[JSON] [ExchangeRate]

getExchangeRates
    :: ApiToken
    -> ClientM [ExchangeRate]

type GetExchangeRateApi =
    AuthorizationHeader
    :> "rates"
    :> QueryParam' [Strict, Required] "source" Currency
    :> QueryParam' [Strict, Required] "target" Currency
    :> QueryParam' [Strict, Optional] "time"   UTCTime
    :> Get '[JSON] [ExchangeRate]

getExchangeRate
    :: ApiToken
    -> Currency
    -> Currency
    -> Maybe UTCTime
    -> ClientM [ExchangeRate]

type GetExchangeRateOverTimeApi =
    AuthorizationHeader
    :> "rates"
    :> QueryParam' [Strict, Required] "source" Currency
    :> QueryParam' [Strict, Required] "target" Currency
    :> QueryParam' [Strict, Required] "from"   UTCTime
    :> QueryParam' [Strict, Required] "to"     UTCTime
    :> QueryParam' [Strict, Optional] "group"  Grouping
    :> Get '[JSON] [ExchangeRate]

getExchangeRateOverTime
    :: ApiToken
    -> Currency
    -> Currency
    -> UTCTime
    -> UTCTime
    -> Maybe Grouping
    -> ClientM [ExchangeRate]

----------------------------------------------------------------------------------------------------
-- Profiles

type GetProfilesApi =
    AuthorizationHeader
    :> "profiles"
    :> Get '[JSON] [Profile]

getProfiles
    :: ApiToken
    -> ClientM [Profile]

----------------------------------------------------------------------------------------------------
-- Quote

type CreateQuoteApi =
    AuthorizationHeader
    :> "quotes"
    :> ReqBody '[JSON] CreateQuote
    :> Post '[JSON] Quote

createQuote
    :: ApiToken
    -> CreateQuote
    -> ClientM Quote

type CreateTemporaryQuoteApi =
    AuthorizationHeader
    :> "quotes"
    :> QueryParam' [Strict, Required] "source"       Currency
    :> QueryParam' [Strict, Required] "target"       Currency
    :> QueryParam' [Strict, Optional] "sourceAmount" Amount
    :> QueryParam' [Strict, Optional] "targetAmount" Amount
    :> QueryParam' [Strict, Required] "rateType"     RateType
    :> Get '[JSON] TemporaryQuote

createTemporaryQuote
    :: ApiToken
    -> Currency
    -> Currency
    -> Maybe Amount
    -> Maybe Amount
    -> RateType
    -> ClientM TemporaryQuote

----------------------------------------------------------------------------------------------------
-- Accounts

type GetAccountsApi =
    AuthorizationHeader
    :> "borderless-accounts"
    :> QueryParam' [Strict, Required] "profileId" ProfileId
    :> Get '[JSON] [Account]

getAccounts
    :: ApiToken
    -> ProfileId
    -> ClientM [Account]

----------------------------------------------------------------------------------------------------
-- Servant client

type V1Api =
    GetExchangeRatesApi
    :<|> GetExchangeRateApi
    :<|> GetExchangeRateOverTimeApi
    :<|> GetProfilesApi
    :<|> CreateQuoteApi
    :<|> CreateTemporaryQuoteApi
    :<|> GetAccountsApi

type Api = "v1" :> V1Api

getExchangeRates
    :<|> getExchangeRate
    :<|> getExchangeRateOverTime
    :<|> getProfiles
    :<|> createQuote
    :<|> createTemporaryQuote
    :<|> getAccounts
    = client @Api Proxy
