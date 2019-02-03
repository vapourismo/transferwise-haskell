{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.Transferwise.Types
    ( ApiToken (..)
    , Currency (..)
    , ExchangeRate (..)
    , Grouping (..)
    , ProfileId (..)
    , Profile (..)
    , AddressId (..)
    )
where

import           Data.Aeson      ((.:))
import qualified Data.Aeson      as Aeson
import           Data.ByteString (ByteString)
import           Data.Scientific (Scientific)
import           Data.String     (IsString)
import           Data.Text       (Text)
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
    { exhangeRateSource :: Currency
    , exhangeRateTarget :: Currency
    , exhangeRateValue  :: Scientific
    , exhangeRateTime   :: UTCTime
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
    deriving (Show, Eq, Ord, Aeson.FromJSON, Aeson.ToJSON)

data ProfileType
    = Personal
    | Business
    deriving (Show, Eq, Ord, Bounded, Enum)

instance Aeson.FromJSON ProfileType where
    parseJSON (Aeson.String "personal") = pure Personal
    parseJSON (Aeson.String "business") = pure Business
    parseJSON _                         = fail "Profile type needs to be 'personal' or 'business'"

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
