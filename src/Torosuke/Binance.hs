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
data BiKline = Kline
  { biOpenT :: UTCTime,
    biOpen :: Float,
    biHigh :: Float,
    biLow :: Float,
    biClose :: Float,
    biVolume :: Float,
    biCloseT :: UTCTime
  }
  deriving (Show)

newtype BiKlines = Klines [BiKline] deriving (Generic, Show)

instance FromJSON BiKlines

instance FromJSON BiKline where
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

getKlinesURL :: String -> String -> Int -> String
getKlinesURL pair interval limit' =
  let limit = show limit'
   in "https://api.binance.com/api/v3/klines?symbol="
        ++ pair
        ++ "&interval="
        ++ interval
        ++ "&limit="
        ++ limit

getKlines :: String -> String -> Int -> IO (Maybe BiKlines)
getKlines pair interval limit = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest $ getKlinesURL pair interval limit
  response <- httpLbs request manager
  pure $ decode $ responseBody response
