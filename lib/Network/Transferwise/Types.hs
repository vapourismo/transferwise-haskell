{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

module Network.Transferwise.Types
    ( ApiToken (..)
    , Currency (..)
    , ExchangeRate (..)
    , Grouping (..)
    , ProfileId (..)
    , Profile (..)
    , AddressId (..)
    , UserId (..)
    , QuoteId (..)
    , QuoteType (..)
    , Quote (..)
    , CreateQuote (..)
    , AccountId (..)
    , Account (..)
    , Balance (..)
    )
where

import           Data.Aeson      ((.:), (.=))
import qualified Data.Aeson      as Aeson
import           Data.ByteString (ByteString)
import           Data.Scientific (Scientific)
import           Data.String     (IsString)
import           Data.Text       (Text, toLower)
import           Data.Time       (Day, UTCTime)

import Servant.API (ToHttpApiData (..))

----------------------------------------------------------------------------------------------------
-- API token

newtype ApiToken = ApiToken {fromApiToken :: ByteString}
    deriving (Eq, Ord)

instance ToHttpApiData ApiToken where
    toHeader (ApiToken auth) = "Bearer " <> auth

    toUrlPiece _ = mempty

    toQueryParam _ = mempty

----------------------------------------------------------------------------------------------------
-- Currency

newtype Currency = Currency {fromCurrency :: Text}
    deriving (Show, Eq, Ord, IsString, Aeson.FromJSON, Aeson.ToJSON)

instance ToHttpApiData Currency where
    toQueryParam = toQueryParam . fromCurrency

----------------------------------------------------------------------------------------------------
-- Exchange rate

data ExchangeRate = ExchangeRate
    { exchangeRateSource :: Currency
    , exchangeRateTarget :: Currency
    , exchangeRateValue  :: Scientific
    , exchangeRateTime   :: UTCTime
    }
    deriving (Show, Eq)

instance Aeson.FromJSON ExchangeRate where
    parseJSON = Aeson.withObject "ExchangeRate" $ \object -> ExchangeRate
        <$> object .: "source"
        <*> object .: "target"
        <*> object .: "rate"
        <*> object .: "time"

----------------------------------------------------------------------------------------------------
-- Result grouping

data Grouping
    = Day
    | Hour
    | TenMinutes
    deriving (Show, Eq, Ord, Bounded, Enum)

instance ToHttpApiData Grouping where
    toQueryParam Day        = "day"
    toQueryParam Hour       = "hour"
    toQueryParam TenMinutes = "minute"

----------------------------------------------------------------------------------------------------
-- Profile

newtype ProfileId = ProfileId Integer
    deriving (Show, Eq, Ord, Aeson.FromJSON, Aeson.ToJSON, ToHttpApiData)

data ProfileType
    = Personal
    | Business
    deriving (Show, Eq, Ord, Bounded, Enum)

instance Aeson.FromJSON ProfileType where
    parseJSON = \case
        Aeson.String (toLower -> "personal") -> pure Personal
        Aeson.String (toLower -> "business") -> pure Business
        _ -> fail "Profile type needs to be 'personal' or 'business'"

instance Aeson.ToJSON ProfileType where
    toJSON Personal = "personal"
    toJSON Business = "business"

data Profile
    = PersonalProfile
        { profileId             :: ProfileId
        , profileFirstName      :: Text
        , profileLastName       :: Text
        , profileDateOfBirth    :: Day
        , profilePhoneNumber    :: Text
        , profilePrimaryAddress :: AddressId
        }
    | BusinessProfile
        { profileId   :: ProfileId
        , profileName :: Text
        }
    deriving (Show, Eq)

instance Aeson.FromJSON Profile where
    parseJSON = Aeson.withObject "Profile" $ \outer -> do
        Aeson.Object details <- outer .: "details"
        profileType <- outer .: "type"
        case profileType of
            Personal -> PersonalProfile
                <$> outer   .: "id"
                <*> details .: "firstName"
                <*> details .: "lastName"
                <*> details .: "dateOfBirth"
                <*> details .: "phoneNumber"
                <*> details .: "primaryAddress"

            Business -> BusinessProfile
                <$> outer   .: "id"
                <*> details .: "name"

----------------------------------------------------------------------------------------------------
-- Address

newtype AddressId = AddressId Integer
    deriving (Show, Eq, Ord, Aeson.FromJSON, Aeson.ToJSON)

----------------------------------------------------------------------------------------------------
-- User

newtype UserId = UserId Integer
    deriving (Show, Eq, Ord, Aeson.FromJSON, Aeson.ToJSON)

----------------------------------------------------------------------------------------------------
-- Quote

data QuoteType
    = BalancePayout
    | BalanceConversion
    | Regular
    deriving (Show, Eq, Ord, Bounded, Enum)

instance Aeson.FromJSON QuoteType where
    parseJSON (Aeson.String quoteType) = case toLower quoteType of
        "balance_payout"     -> pure BalancePayout
        "balance_conversion" -> pure BalanceConversion
        "regular"            -> pure Regular
        _                    -> fail ("Unknown QuoteType: " <> show quoteType)
    parseJSON _ = fail "QuoteType must be a string"

instance Aeson.ToJSON QuoteType where
    toJSON BalancePayout     = "BALANCE_PAYOUT"
    toJSON BalanceConversion = "BALANCE_CONVERSION"
    toJSON Regular           = "REGULAR"

data CreateQuote = CreateQuote
    { createQuoteProfile :: ProfileId
    , createQuoteSource  :: Currency
    , createQuoteTarget  :: Currency
    , createQuoteAmount  :: Either Scientific Scientific
    , createQuoteType    :: QuoteType
    }
    deriving (Show, Eq)

instance Aeson.ToJSON CreateQuote where
    toJSON quote = Aeson.object
        [ "profile"  .= createQuoteProfile quote
        , "source"   .= createQuoteSource quote
        , "target"   .= createQuoteTarget quote
        , amountField
        , "rateType" .= ("FIXED" :: Text)
        , "type"     .= createQuoteType quote
        ]
        where
            amountField = case createQuoteAmount quote of
                Left amount  -> "sourceAmount" .= amount
                Right amount -> "targetAmount" .= amount

newtype QuoteId = QuoteId Integer
    deriving (Show, Eq, Ord, Aeson.FromJSON, Aeson.ToJSON)

data Quote = Quote
    { quoteId                     :: QuoteId
    , quoteSource                 :: Currency
    , quoteTarget                 :: Currency
    , quoteSourceAmount           :: Scientific
    , quoteTargetAmount           :: Scientific
    , quoteType                   :: QuoteType
    , quoteRate                   :: Scientific
    , quoteCreatedTime            :: UTCTime
    , quoteCreatedByUserId        :: UserId
    , quoteProfile                :: ProfileId
    , quoteDeliveryEstimate       :: UTCTime
    , quoteFee                    :: Scientific
    , quoteAllowedProfileTypes    :: [ProfileType]
    , quoteGuaranteedTargetAmount :: Bool
    , quoteOfSourceAmount         :: Bool
    }
    deriving (Show, Eq)

instance Aeson.FromJSON Quote where
    parseJSON = Aeson.withObject "Quote" $ \object -> Quote
        <$> object .: "id"
        <*> object .: "source"
        <*> object .: "target"
        <*> object .: "sourceAmount"
        <*> object .: "targetAmount"
        <*> object .: "type"
        <*> object .: "rate"
        <*> object .: "createdTime"
        <*> object .: "createdByUserId"
        <*> object .: "profile"
        <*> object .: "deliveryEstimate"
        <*> object .: "fee"
        <*> object .: "allowedProfileTypes"
        <*> object .: "guaranteedTargetAmount"
        <*> object .: "ofSourceAmount"

----------------------------------------------------------------------------------------------------
-- Accounts

newtype AccountId = AccountId Integer
    deriving (Show, Eq, Ord, Aeson.FromJSON, Aeson.ToJSON)

data Balance = Balance
    { balanceCurrency               :: Currency
    , balanceAmountValue            :: Scientific
    , balanceAmountCurrency         :: Currency
    , balanceReservedAmountValue    :: Scientific
    , balanceReservedAmountCurrency :: Currency
    }
    deriving (Show, Eq)

instance Aeson.FromJSON Balance where
    parseJSON = Aeson.withObject "Balance" $ \object -> do
        Aeson.Object amount   <- object .: "amount"
        Aeson.Object reserved <- object .: "reservedAmount"
        Balance
            <$> object   .: "currency"
            <*> amount   .: "value"
            <*> amount   .: "currency"
            <*> reserved .: "value"
            <*> reserved .: "currency"

data Account = Account
    { accountId        :: AccountId
    , accountProfileId :: ProfileId
    , accountActive    :: Bool
    , accountBalances  :: [Balance]
    }
    deriving (Show, Eq)

instance Aeson.FromJSON Account where
    parseJSON = Aeson.withObject "Account" $ \object -> Account
        <$> object .: "id"
        <*> object .: "profileId"
        <*> object .: "active"
        <*> object .: "balances"
