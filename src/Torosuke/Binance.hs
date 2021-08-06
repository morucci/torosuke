{-# LANGUAGE DeriveGeneric #-}

module Torosuke.Binance where

import Data.Aeson (FromJSON, decode, parseJSON, withArray, withScientific, withText)
import Data.Aeson.Types (parseFail)
import qualified Data.Scientific as S
import Data.Time.Clock
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Network.HTTP.Client
  ( Response (responseBody),
    httpLbs,
    newManager,
    parseRequest,
    responseBody,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Relude

-- https://github.com/binance/binance-spot-api-docs/blob/master/rest-api.md#klinecandlestick-data
data Kline = Kline
  { openT :: UTCTime,
    open :: Float,
    high :: Float,
    low :: Float,
    close :: Float,
    volume :: Float,
    closeT :: UTCTime
  }
  deriving (Show)

newtype Klines = Klines [Kline] deriving (Generic, Show)

instance FromJSON Klines

fakeDate :: UTCTime
fakeDate = fromMaybe (error "nop") (readMaybe "2021-05-31 10:00:00 Z")

instance FromJSON Kline where
  parseJSON = withArray "kline" $ \v -> do
    -- let v' = trace (show v) v
    case toList v of
      [openT, open, high, low, close, volume, closeT, _, _, _, _, _] ->
        Kline
          <$> sciToD openT
          <*> sTof open
          <*> sTof high
          <*> sTof low
          <*> sTof close
          <*> sTof volume
          <*> sciToD closeT
      otherValue -> parseFail $ "Unexpected amount of elements:" ++ show otherValue
    where
      sTof = withText "sTof" $ \v -> do
        case readMaybe $ toString v :: Maybe Float of
          Just v' -> pure v'
          Nothing -> parseFail "Unable to parse float"
      sciToD = withScientific "sciToD" $ \v -> do
        case parseDateValue <$> getInt v of
          Just (Just v') -> pure v'
          _ -> parseFail "Unable to parse date"
        where
          getInt :: S.Scientific -> Maybe Int
          getInt s = case S.floatingOrInteger s of
            Right i -> Just i
            Left _ -> Nothing

parseDateValue :: Int -> Maybe UTCTime
parseDateValue epoch = tryParse "%s" <|> tryParse "%Es"
  where
    tryParse fmt = parseTimeM False defaultTimeLocale fmt (conv epoch)
    conv :: Int -> String
    conv i = show $ round $ fromIntegral i / 1000

getKlinesURL :: String -> String -> String
getKlinesURL pair interval = "https://api.binance.com/api/v3/klines?symbol=" ++ pair ++ "&interval=" ++ interval ++ "&limit=" ++ "10"

getKlines :: IO (Maybe Klines)
getKlines = do
  manager <- newManager tlsManagerSettings
  initReq <- parseRequest $ getKlinesURL "ADAUSDT" "1h"
  let request = initReq
  response <- httpLbs request manager
  -- print (responseBody response)
  let decoded = decode $ responseBody response
  print decoded
  pure decoded
