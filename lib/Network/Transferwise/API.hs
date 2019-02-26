{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

-- | Servant Client types and functions
module Network.Transferwise.API where

import Data.Proxy (Proxy (..))
import Data.Text  (Text)
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
    :> QueryParam' [Strict, Required] "source" Text
    :> QueryParam' [Strict, Required] "target" Text
    :> QueryParam' [Strict, Optional] "time"   UTCTime
    :> Get '[JSON] [ExchangeRate]

getExchangeRate
    :: ApiToken
    -> Text
    -> Text
    -> Maybe UTCTime
    -> ClientM [ExchangeRate]

type GetExchangeRateOverTimeApi =
    AuthorizationHeader
    :> "rates"
    :> QueryParam' [Strict, Required] "source" Text
    :> QueryParam' [Strict, Required] "target" Text
    :> QueryParam' [Strict, Required] "from"   UTCTime
    :> QueryParam' [Strict, Required] "to"     UTCTime
    :> QueryParam' [Strict, Optional] "group"  Grouping
    :> Get '[JSON] [ExchangeRate]

getExchangeRatesDuring
    :: ApiToken
    -> Text
    -> Text
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
    :> QueryParam' [Strict, Required] "source"       Text
    :> QueryParam' [Strict, Required] "target"       Text
    :> QueryParam' [Strict, Optional] "sourceAmount" Amount
    :> QueryParam' [Strict, Optional] "targetAmount" Amount
    :> QueryParam' [Strict, Required] "rateType"     RateType
    :> Get '[JSON] TempQuote

createTemporaryQuote
    :: ApiToken
    -> Text
    -> Text
    -> Maybe Amount
    -> Maybe Amount
    -> RateType
    -> ClientM TempQuote

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
    :<|> getExchangeRatesDuring
    :<|> getProfiles
    :<|> createQuote
    :<|> createTemporaryQuote
    :<|> getAccounts
    = client @Api Proxy
