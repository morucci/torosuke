module Torosuke.Binance where

import Data.Aeson (FromJSON, decode, parseJSON, withArray, withScientific, withText)
import Data.Aeson.Types (parseFail)
import qualified Data.Scientific as S
import Data.Time.Clock
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Network.HTTP.Client
  ( Response (responseBody),
    httpLbs,
    newManager,
    parseRequest,
    responseBody,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Relude
import Torosuke.Common.Types

-- https://github.com/binance/binance-spot-api-docs/blob/master/rest-api.md#klinecandlestick-data
data BiKline = BiKline
  { biOpenT :: UTCTime,
    biOpen :: Float,
    biHigh :: Float,
    biLow :: Float,
    biClose :: Float,
    biVolume :: Float,
    biCloseT :: UTCTime
  }
  deriving (Show)

toKline :: BiKline -> Kline
toKline BiKline {..} =
  Kline
    { openT = biOpenT,
      open = biOpen,
      high = biHigh,
      low = biLow,
      close = biClose,
      volume = biVolume,
      closeT = biCloseT
    }

toKlines :: BiKlines -> Klines
toKlines bkls = Klines (toKline <$> biGet bkls)
  where
    biGet (BiKlines kls) = kls

newtype BiKlines = BiKlines [BiKline] deriving (Generic, Show)

instance FromJSON BiKlines

instance FromJSON BiKline where
  parseJSON = withArray "kline" $ \v -> do
    -- let v' = trace (show v) v
    case toList v of
      [openT, open, high, low, close, volume, closeT, _, _, _, _, _] ->
        BiKline
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
          getInt s = case (S.floatingOrInteger s :: Either Float Int) of
            Right i -> Just i
            Left _ -> Nothing

parseDateValue :: Int -> Maybe UTCTime
parseDateValue epoch = tryParse "%s" <|> tryParse "%Es"
  where
    tryParse fmt = parseTimeM False defaultTimeLocale fmt (conv epoch)
    conv :: Int -> String
    conv i = show (round (fromIntegral i / 1000 :: Float) :: Int)

getKlinesURL :: String -> String -> Int -> Maybe UTCTime -> String
getKlinesURL pair interval limit' endTM' =
  let limit = show limit'
      endT = case endTM' of
        Just endT' -> "&endTime=" ++ formatTime defaultTimeLocale "%s" endT' ++ "000"
        Nothing -> ""
   in "https://api.binance.com/api/v3/klines?symbol="
        ++ pair
        ++ "&interval="
        ++ interval
        ++ "&limit="
        ++ limit
        ++ endT

adate :: UTCTime
adate = fromMaybe (error "nop") (readMaybe "2020-01-01 00:00:00 Z" :: Maybe UTCTime)

getKlines :: String -> String -> Int -> Maybe UTCTime -> IO (Maybe Klines)
getKlines pair interval limit endTM = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest $ getKlinesURL pair interval limit endTM
  response <- httpLbs request manager
  let decoded = decode $ responseBody response :: Maybe BiKlines
  pure $ toKlines <$> decoded
