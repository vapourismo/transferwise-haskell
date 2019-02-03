{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Network.Transferwise.API
    ( -- * Servant basics
      transferwiseApiBaseUrl

      -- * Exchange rates
    , getExchangeRates
    , getExchangeRate
    , getExchangeRateOverTime

      -- * Profiles
    , getProfiles
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
    :> Get '[JSON] ExchangeRate

getExchangeRate
    :: ApiToken
    -> Currency
    -> Currency
    -> Maybe UTCTime
    -> ClientM ExchangeRate

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
-- Servant client

type V1Api =
    GetExchangeRatesApi
    :<|> GetExchangeRateApi
    :<|> GetExchangeRateOverTimeApi
    :<|> GetProfilesApi

type Api = "v1" :> V1Api

getExchangeRates
    :<|> getExchangeRate
    :<|> getExchangeRateOverTime
    :<|> getProfiles
    = client @Api Proxy
