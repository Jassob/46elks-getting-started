--
-- 46elks API Sample
-- Sending SMS using Haskell and the 46elks API
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Monoid ((<>))
import GHC.Generics (Generic)
import Network.HTTP.Simple ( Request, parseRequest, httpJSON
                           , setRequestBasicAuth, setRequestBodyURLEncoded
                           , setRequestMethod, getResponseBody)

-- | SMS data type for 46elks API, the compiler will automatically
-- derive instances for transforming an SMS to a JSON object.
data SMS = SMS { smsTo :: String
               , smsFrom :: String
               , smsMessage :: String
               } deriving (Show)

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

type Username = ByteString
type Secret   = ByteString

-- | Prepares a SMS for inclusion in a URLEncoded POST body.
urlEncodeSMS :: SMS -> [(ByteString, ByteString)]
urlEncodeSMS (SMS to from message) = [ ("to", pack to)
                                     , ("from", pack from)
                                     , ("message", pack message) ]

-- Your 46elks API key
username :: Username
username = "YOUR_API_USER_IDENTIFIER"

-- Your 46elks API secret
secret :: Secret
secret = "YOUR_API_SECRET"

-- Format the API URL to use for the request
apiUrl :: String
apiUrl = "https://api.46elks.com/a1/SMS"

-- | Prepares a request by making it a POST request, with JSON
-- encoding and Basic authentication.
prepareRequest :: SMS -> Request -> Request
prepareRequest sms req =
  setRequestBodyURLEncoded (urlEncodeSMS sms)
  . setRequestMethod "POST"
  . setRequestBasicAuth username secret
  $ req

-- | Sends an sms to a number through 463elks api.
send_sms :: IO ()
send_sms = do
    -- Send SMS POST to 46elks
    let sms = SMS "+46700000000" "Haskelk" "Hello from Haskell"
    request <- prepareRequest sms <$> parseRequest apiUrl

    -- Parse the result for pretty printing
    body <- getResponseBody <$> httpJSON request

    -- Print response message
    putStrLn $ responseString (message body) (from body) (to body)

      where responseString :: String -> String -> String -> String
            responseString msg sender recipient =
              "Sent \"" <> msg <> "\" from " <> sender <> " to " <> recipient

main :: IO ()
main = send_sms
