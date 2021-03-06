module Torosuke.Binance where

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Retry (RetryStatus (..))
import qualified Control.Retry as Retry
import Data.Aeson (FromJSON, decode, parseJSON, withArray, withScientific, withText)
import Data.Aeson.Types (parseFail)
import qualified Data.Scientific as S
import Data.Time.Clock
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Network.HTTP.Client
  ( HttpException (..),
    Response (responseBody, responseHeaders, responseStatus),
    host,
    httpLbs,
    newManager,
    parseRequest,
    path,
    port,
    queryString,
    responseBody,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Relude
import Torosuke.Types

-- https://github.com/binance/binance-spot-api-docs/blob/master/rest-api.md#klinecandlestick-data
data BiKline = BiKline
  { biOpenT :: UTCTime,
    biOpen :: Double,
    biHigh :: Double,
    biLow :: Double,
    biClose :: Double,
    biVolume :: Double,
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
        case readMaybe $ toString v :: Maybe Double of
          Just v' -> pure v'
          Nothing -> parseFail "Unable to parse float"
      sciToD = withScientific "sciToD" $ \v -> do
        case parseDateValue <$> getInt v of
          Just (Just v') -> pure v'
          _ -> parseFail "Unable to parse date"
        where
          getInt :: S.Scientific -> Maybe Int
          getInt s = case (S.floatingOrInteger s :: Either Double Int) of
            Right i -> Just i
            Left _ -> Nothing

parseDateValue :: Int -> Maybe UTCTime
parseDateValue epoch = tryParse "%s" <|> tryParse "%Es"
  where
    tryParse fmt = parseTimeM False defaultTimeLocale fmt (conv epoch)
    conv :: Int -> String
    conv i = show (round (fromIntegral i / 1000 :: Float) :: Int)

-- See for limit (weight of 1200 / minutes)
-- https://api.binance.com/api/v3/exchangeInfo
-- See in response header: x-mbx-used-weight-1m
-- Also more info in https://dev.binance.vision/t/what-are-the-ip-weights/280

getKlinesURL :: MonadReader Env m => Int -> Maybe UTCTime -> m String
getKlinesURL limit' endTM' = do
  env <- ask
  let limit = show limit'
      endT = case endTM' of
        Just endT' -> "&endTime=" ++ formatTime defaultTimeLocale "%s" endT' ++ "000"
        Nothing -> ""
   in pure $
        "https://api.binance.com/api/v3/klines?symbol="
          ++ pairToText (envPair env)
          ++ "&interval="
          ++ intervalToText (envInterval env)
          ++ "&limit="
          ++ limit
          ++ endT

getKlines :: Int -> Maybe UTCTime -> ReaderT Env IO KlinesHTTPResponse
getKlines limit endTM = do
  manager <- liftIO $ newManager tlsManagerSettings
  url <- getKlinesURL limit endTM
  request <- parseRequest url
  response <- liftIO . retry $ httpLbs request manager
  -- print (show $ responseHeaders response :: Text)
  let decoded = decode $ responseBody response :: Maybe BiKlines
  pure $
    KlinesHTTPResponse
      (responseStatus response)
      (responseHeaders response)
      -- Do not bother with in progress candle
      (toKlines <$> decoded)

retry :: (MonadIO m, MonadMask m) => m a -> m a
retry baseAction =
  Retry.recovering
    (Retry.exponentialBackoff backoff <> Retry.limitRetries 3)
    [handler]
    (const baseAction)
  where
    backoff = 1000000 -- 1s
    handler (RetryStatus num _ _) = Handler $ \case
      HttpExceptionRequest req ctx -> do
        let url = decodeUtf8 $ host req <> ":" <> show (port req) <> path req
            arg = decodeUtf8 $ queryString req
            loc = if num == 0 then url <> arg else url
        putStrLn $ show num <> "/3 " <> loc <> " failed: " <> show ctx
        pure True
      InvalidUrlException _ _ -> pure False
