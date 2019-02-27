{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}

-- | Request and response types
module Network.Transferwise.Types where

import           Data.Aeson       ((.:), (.=))
import qualified Data.Aeson       as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.ByteString  (ByteString)
import           Data.Hashable    (Hashable)
import           Data.String      (IsString (fromString))
import           Data.Text        (Text, toLower, toUpper)
import           Data.Time        (Day, UTCTime)

import qualified Money

import Servant.API (ToHttpApiData (..))

newtype ApiToken = ApiToken {fromApiToken :: ByteString}
    deriving (Eq, Ord, IsString, Hashable)

instance ToHttpApiData ApiToken where
    toHeader (ApiToken auth) = "Bearer " <> auth

    toUrlPiece _ = mempty -- We don't want to leak this by accident.

    toQueryParam _ = mempty  -- We don't want to leak this by accident.

toSomeDense :: Text -> Rational -> Aeson.Parser Money.SomeDense
toSomeDense currency notional =
    maybe
        (fail ("Failed to construct Money.SomeDense for " <> show (currency, notional)))
        pure
        (Money.mkSomeDense currency notional)

toSomeExchangeRate :: Text -> Text -> Rational -> Aeson.Parser Money.SomeExchangeRate
toSomeExchangeRate payCcy receiveCcy rate =
    maybe
        (fail ("Failed to construct Money.SomeExchangeRate for " <> show (payCcy, receiveCcy, rate)))
        pure
        (Money.mkSomeExchangeRate payCcy receiveCcy rate)

data ExchangeRate = ExchangeRate
    { exchangeRateValue :: Money.SomeExchangeRate
    , exchangeRateTime  :: UTCTime
    }
    deriving (Show, Eq)

instance Aeson.FromJSON ExchangeRate where
    parseJSON = Aeson.withObject "ExchangeRate" $ \object -> do
        source <- object .: "source"
        target <- object .: "target"
        rate   <- object .: "rate"
        ExchangeRate <$> toSomeExchangeRate source target (toRational @Double rate) <*> object .: "time"

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
        -- TODO: Add more fields to the business profile constructor
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

newtype Amount = Amount Rational
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
    toHeader (Amount value) = fromString (show (fromRational value :: Double))

    toUrlPiece (Amount value) = fromString (show (fromRational value :: Double))

    toQueryParam (Amount value) = fromString (show (fromRational value :: Double))

data RateType
    = Fixed
    deriving (Show, Eq, Ord, Bounded, Enum)

instance ToHttpApiData RateType where
    toHeader     _ = "FIXED"
    toUrlPiece   _ = "FIXED"
    toQueryParam _ = "FIXED"

instance Aeson.ToJSON RateType where
    toJSON Fixed = "FIXED"

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

data Conversion
    = ConvertFrom
        { conversionPay             :: Money.SomeDense
        , conversionReceiveCurrency :: Text
        }
    | ConvertTo
        { conversionPayCurrency :: Text
        , conversionReceive     :: Money.SomeDense
        }
    deriving (Show, Eq)

data CreateQuote = CreateQuote
    { createQuoteProfile    :: ProfileId
    , createQuoteType       :: QuoteType
    , createQuoteConversion :: Conversion
    }
    deriving (Show, Eq)

instance Aeson.ToJSON CreateQuote where
    toJSON quote = Aeson.object $
        ("profile"    .= createQuoteProfile quote)
        : ("rateType" .= Fixed)
        : ("type"     .= createQuoteType quote)
        : amountField
        where
            amountField = case createQuoteConversion quote of
                ConvertFrom pay receiveCcy ->
                    [ "source"       .= Money.someDenseCurrency pay
                    , "sourceAmount" .= Money.someDenseAmount pay
                    , "target"       .= receiveCcy
                    ]

                ConvertTo payCcy receive ->
                    [ "source"       .= payCcy
                    , "target"       .= Money.someDenseCurrency receive
                    , "targetAmount" .= Money.someDenseAmount receive
                    ]

newtype QuoteId = QuoteId Integer
    deriving (Show, Eq, Ord, Aeson.FromJSON, Aeson.ToJSON, Hashable)

data Quote = Quote
    { quoteId                     :: QuoteId
    , quoteSource                 :: Money.SomeDense
    , quoteTarget                 :: Money.SomeDense
    , quoteType                   :: QuoteType
    , quoteRate                   :: Money.SomeExchangeRate
    , quoteCreatedTime            :: UTCTime
    , quoteCreatedByUserId        :: UserId
    , quoteProfile                :: ProfileId
    , quoteDeliveryEstimate       :: UTCTime
    , quoteFee                    :: Money.SomeDense
    , quoteAllowedProfileTypes    :: [ProfileType]
    , quoteGuaranteedTargetAmount :: Bool
    , quoteOfSourceAmount         :: Bool
    }
    deriving (Show, Eq)

instance Aeson.FromJSON Quote where
    parseJSON = Aeson.withObject "Quote" $ \object -> do
        source       <- object .: "source"
        target       <- object .: "target"
        sourceAmount <- object .: "sourceAmount"
        targetAmount <- object .: "targetAmount"
        fee          <- object .: "fee"
        rate         <- object .: "rate"

        Quote
            <$> object .: "id"
            <*> toSomeDense source (toRational @Double sourceAmount)
            <*> toSomeDense target (toRational @Double targetAmount)
            <*> object .: "type"
            <*> toSomeExchangeRate source target (toRational @Double rate)
            <*> object .: "createdTime"
            <*> object .: "createdByUserId"
            <*> object .: "profile"
            <*> object .: "deliveryEstimate"
            <*> toSomeDense source (toRational @Double fee)
            <*> object .: "allowedProfileTypes"
            <*> object .: "guaranteedTargetAmount"
            <*> object .: "ofSourceAmount"

data TempQuote = TempQuote
    { tempQuoteSource                 :: Money.SomeDense
    , tempQuoteTarget                 :: Money.SomeDense
    , tempQuoteType                   :: QuoteType
    , tempQuoteRate                   :: Money.SomeExchangeRate
    , tempQuoteCreatedTime            :: UTCTime
    , tempQuoteDeliveryEstimate       :: UTCTime
    , tempQuoteFee                    :: Money.SomeDense
    , tempQuoteAllowedProfileTypes    :: [ProfileType]
    , tempQuoteGuaranteedTargetAmount :: Bool
    , tempQuoteOfSourceAmount         :: Bool
    }
    deriving (Show, Eq)

instance Aeson.FromJSON TempQuote where
    parseJSON = Aeson.withObject "TempQuote" $ \object -> do
        source       <- object .: "source"
        target       <- object .: "target"
        sourceAmount <- object .: "sourceAmount"
        targetAmount <- object .: "targetAmount"
        fee          <- object .: "fee"
        rate         <- object .: "rate"

        TempQuote
            <$> toSomeDense source (toRational @Double sourceAmount)
            <*> toSomeDense target (toRational @Double targetAmount)
            <*> object .: "type"
            <*> toSomeExchangeRate source target (toRational @Double rate)
            <*> object .: "createdTime"
            <*> object .: "deliveryEstimate"
            <*> toSomeDense source (toRational @Double fee)
            <*> object .: "allowedProfileTypes"
            <*> object .: "guaranteedTargetAmount"
            <*> object .: "ofSourceAmount"

newtype AccountId = AccountId Integer
    deriving (Show, Eq, Ord, Aeson.FromJSON, Aeson.ToJSON, Hashable)

data Balance = Balance
    { balanceAmount         :: Money.SomeDense
    , balanceReservedAmount :: Money.SomeDense
    }
    deriving (Show, Eq)

parseSomeDense :: Aeson.Value -> Aeson.Parser Money.SomeDense
parseSomeDense = Aeson.withObject "Money.SomeDense" $ \amount -> do
    currency <- amount .: "currency"
    notional <- amount .: "value"
    toSomeDense currency (toRational @Double notional)

instance Aeson.FromJSON Balance where
    parseJSON = Aeson.withObject "Balance" $ \object -> do
        amountVal   <- object .: "amount"
        reservedVal <- object .: "reservedAmount"
        Balance <$> parseSomeDense amountVal <*> parseSomeDense reservedVal

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
