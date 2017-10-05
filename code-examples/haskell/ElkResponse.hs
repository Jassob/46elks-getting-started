-- | A module containing the responses from 46elks. The data types
-- reside in this module because of the naming clash between the
-- fields in ElkResponse data type and the SMS data type.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ElkResponse where

import           Data.Aeson (withObject, (.:), FromJSON(..))
import qualified Data.Aeson.Types as T
import           GHC.Generics

-- | Response type on a successful POST of an SMS to 46elks.
data ElkSMSResponse = Resp
  { status :: String
  , direction :: String
  , from :: String
  , created :: String
  , parts :: Int
  , to :: String
  , cost :: Int
  , message :: String
  , id :: String
  } deriving (Show, Generic, FromJSON)

-- | Response type when sending a GET request to
-- https://api.46elks.com/a1/SMS.
data ElkSMSResponses = Resps { resps :: [ElkSMSResponse] }

instance FromJSON ElkSMSResponses where
  parseJSON = withObject "Responses" $ \o -> Resps <$>  o .: "data"
