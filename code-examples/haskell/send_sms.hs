--
-- 46elks API Sample
-- Sending SMS using Haskell and the 46elks API
--
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import GHC.Generics
import Network.HTTP.Simple ( Request, parseRequest, httpJSON
                           , setRequestBasicAuth, setRequestBodyJSON
                           , setRequestMethod, getResponseBody)

import ElkResponse

-- Information about how to porse the response code from 46elks API
data SMS = SMS { to :: String
               , from :: String
               , message :: String
               } deriving (Show, Generic, FromJSON, ToJSON)

type Username = ByteString
type Secret   = ByteString

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
  setRequestBodyJSON sms
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
    (msg, from, to) <- smsInfo . getResponseBody <$> httpJSON request
    putStrLn $ "Sent \"" <> msg <> "\" from " <> from <> " to " <> to
      where smsInfo :: ElkSMSResponse -> (String, String, String)
            smsInfo resp = ( ElkResponse.message resp
                           , ElkResponse.from resp
                           , ElkResponse.to resp
                           )

main :: IO ()
main = send_sms
