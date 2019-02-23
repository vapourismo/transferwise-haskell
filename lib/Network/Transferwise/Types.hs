{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

-- | Request and response types
module Network.Transferwise.Types where

import           Data.Aeson      ((.:), (.=))
import qualified Data.Aeson      as Aeson
import           Data.ByteString (ByteString)
import           Data.Hashable   (Hashable)
import           Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import           Data.String     (IsString (fromString))
import           Data.Text       (Text, toLower, toUpper)
import           Data.Time       (Day, UTCTime)

import Servant.API (ToHttpApiData (..))

newtype ApiToken = ApiToken {fromApiToken :: ByteString}
    deriving (Eq, Ord, IsString, Hashable)

instance ToHttpApiData ApiToken where
    toHeader (ApiToken auth) = "Bearer " <> auth

    toUrlPiece _ = mempty -- We don't want to leak this by accident.

    toQueryParam _ = mempty  -- We don't want to leak this by accident.

newtype Currency = Currency {fromCurrency :: Text}
    deriving (Show, Eq, Ord, IsString, Aeson.FromJSON, Aeson.ToJSON, Hashable)

instance ToHttpApiData Currency where
    toQueryParam = toQueryParam . fromCurrency

data ExchangeRate = ExchangeRate
    { -- | Pay currency
      exchangeRateSource :: Currency

    , -- | Receive currency
      exchangeRateTarget :: Currency

    , -- | Value of the pay currency expressed in the receive currency
      exchangeRateValue  :: Scientific

    , -- | Time stamp
      exchangeRateTime   :: UTCTime
    }
    deriving (Show, Eq)

instance Aeson.FromJSON ExchangeRate where
    parseJSON = Aeson.withObject "ExchangeRate" $ \object -> ExchangeRate
        <$> object .: "source"
        <*> object .: "target"
        <*> object .: "rate"
        <*> object .: "time"

data Grouping
    = Day
    | Hour
    | TenMinutes
    deriving (Show, Eq, Ord, Bounded, Enum)

instance ToHttpApiData Grouping where
    toQueryParam Day        = "day"
    toQueryParam Hour       = "hour"
    toQueryParam TenMinutes = "minute"

newtype ProfileId = ProfileId Integer
    deriving (Show, Eq, Ord, Aeson.FromJSON, Aeson.ToJSON, ToHttpApiData, Hashable)

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

newtype AddressId = AddressId Integer
    deriving (Show, Eq, Ord, Aeson.FromJSON, Aeson.ToJSON, Hashable)

newtype UserId = UserId Integer
    deriving (Show, Eq, Ord, Aeson.FromJSON, Aeson.ToJSON, Hashable)

newtype Amount = Amount Scientific
    deriving
        ( Show
        , Eq
        , Ord
        , Aeson.FromJSON
        , Aeson.ToJSON
        , Hashable
        , Num
        , Real
        , RealFrac
        , Fractional
        )

instance ToHttpApiData Amount where
    toHeader (Amount value) =
        fromString (Scientific.formatScientific Scientific.Fixed Nothing value)

    toUrlPiece (Amount value) =
        fromString (Scientific.formatScientific Scientific.Fixed Nothing value)

    toQueryParam (Amount value) =
        fromString (Scientific.formatScientific Scientific.Fixed Nothing value)


data RateType
    = Fixed
    deriving (Show, Eq, Ord, Bounded, Enum)

instance ToHttpApiData RateType where
    toHeader     _ = "FIXED"
    toUrlPiece   _ = "FIXED"
    toQueryParam _ = "FIXED"

data QuoteType
    = BalancePayout
    | BalanceConversion
    | Regular
    deriving (Show, Eq, Ord, Bounded, Enum)

instance Aeson.FromJSON QuoteType where
    parseJSON (Aeson.String quoteType) = case toUpper quoteType of
        "BALANCE_PAYOUT"     -> pure BalancePayout
        "BALANCE_CONVERSION" -> pure BalanceConversion
        "REGULAR"            -> pure Regular
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
    deriving (Show, Eq, Ord, Aeson.FromJSON, Aeson.ToJSON, Hashable)

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

data TempQuote = TempQuote
    { tempQuoteSource                 :: Currency
    , tempQuoteTarget                 :: Currency
    , tempQuoteSourceAmount           :: Scientific
    , tempQuoteTargetAmount           :: Scientific
    , tempQuoteType                   :: QuoteType
    , tempQuoteRate                   :: Scientific
    , tempQuoteCreatedTime            :: UTCTime
    , tempQuoteDeliveryEstimate       :: UTCTime
    , tempQuoteFee                    :: Scientific
    , tempQuoteAllowedProfileTypes    :: [ProfileType]
    , tempQuoteGuaranteedTargetAmount :: Bool
    , tempQuoteOfSourceAmount         :: Bool
    }
    deriving (Show, Eq)

instance Aeson.FromJSON TempQuote where
    parseJSON = Aeson.withObject "TempQuote" $ \object -> TempQuote
        <$> object .: "source"
        <*> object .: "target"
        <*> object .: "sourceAmount"
        <*> object .: "targetAmount"
        <*> object .: "type"
        <*> object .: "rate"
        <*> object .: "createdTime"
        <*> object .: "deliveryEstimate"
        <*> object .: "fee"
        <*> object .: "allowedProfileTypes"
        <*> object .: "guaranteedTargetAmount"
        <*> object .: "ofSourceAmount"

newtype AccountId = AccountId Integer
    deriving (Show, Eq, Ord, Aeson.FromJSON, Aeson.ToJSON, Hashable)

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
