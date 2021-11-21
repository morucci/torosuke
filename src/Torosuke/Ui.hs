module Torosuke.Ui where

import Brick
import Brick.BChan
import Brick.Widgets.Table (renderTable, table)
import Control.Concurrent (forkIO, threadDelay)
import Data.Time.Format
import qualified Graphics.Vty as V
import Numeric
import Relude
import Torosuke.Store (loadAllPairAnalysis)
import Torosuke.Types
  ( Analysis (aCloseT, aKlines, aMacd, aMacdAnalisys),
    AnnotatedAnalysis,
    Kline (close),
    Klines,
    Macd (macdLine, signalLine),
    MacdAnalysis (maMVASL, maSLAZ),
    kGet,
  )

newtype Tick = Tick [AnnotatedAnalysis]

data Name = Viewport1 deriving (Eq, Ord, Show)

data AppState = AppState
  { analysis :: [AnnotatedAnalysis],
    ticks :: Int
  }

showFullPrecision :: Int -> Double -> String
showFullPrecision p x = showFFloat (Just p) x ""

app :: App AppState Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

drawUI :: AppState -> [Widget Name]
drawUI s = tableUi
  where
    tableUi =
      [ viewport Viewport1 Vertical $
          renderTable $ table $ [headerRow] <> dataRows
      ]
      where
        headerRow :: [Widget Name]
        headerRow =
          [ str "Pair/Interval",
            str "Last candle date",
            str "MLASL (p-1, p)",
            str "SLAZ (p-1, p)",
            str "ML/SL Diff (p-1, p)",
            str "Close Price (p-1, p)"
          ]
        dataRows :: [[Widget Name]]
        dataRows = analysisToRow <$> analysis s
        analysisToRow :: AnnotatedAnalysis -> [Widget Name]
        analysisToRow ana =
          [ str $ fst (fst ana) <> "/" <> snd (fst ana),
            str $ formatTime defaultTimeLocale "%F %R" $ aCloseT $ snd ana,
            maMVASLToWidget . aMacdAnalisys $ snd ana,
            maSLAZToWidget . aMacdAnalisys $ snd ana,
            macdToWidget . aMacd $ snd ana,
            klinePriceToWidget . aKlines $ snd ana
          ]
        maMVASLToWidget :: MacdAnalysis -> Widget Name
        maMVASLToWidget macdA =
          hBox $ intersperse (str " ") (boolWidget <$> reverse (take 2 (maMVASL macdA)))
        maSLAZToWidget :: MacdAnalysis -> Widget Name
        maSLAZToWidget macdA =
          hBox $ intersperse (str " ") (boolWidget <$> reverse (take 2 (maSLAZ macdA)))
        macdToWidget :: Macd -> Widget Name
        macdToWidget macd = str $ diffFullPrecision diff
          where
            diff :: [Double]
            diff =
              uncurry (-)
                <$> zip
                  (reverse (take 2 (macdLine macd)))
                  (reverse (take 2 (signalLine macd)))
            diffFullPrecision :: [Double] -> String
            diffFullPrecision ld = toString . unwords $ toText . showFullPrecision 10 <$> ld
        klinePriceToWidget :: Klines -> Widget Name
        klinePriceToWidget klines =
          hBox $ intersperse (str " ") (str . showFullPrecision 8 . close <$> reverse (take 2 (kGet klines)))

boolWidget :: Bool -> Widget Name
boolWidget False = withAttr boolFalseAttr $ str "False"
boolWidget True = withAttr boolTrueAttr $ str "True"

boolTrueAttr :: AttrName
boolTrueAttr = "boolTrueAttr"

boolFalseAttr :: AttrName
boolFalseAttr = "boolFalseAttr"

handleEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleEvent s (VtyEvent (V.EvKey V.KEsc [])) = halt s
handleEvent s (VtyEvent (V.EvKey V.KDown [])) = do
  let vp = viewportScroll Viewport1
  vScrollBy vp 1
  continue s
handleEvent s (VtyEvent (V.EvKey V.KUp [])) = do
  let vp = viewportScroll Viewport1
  vScrollBy vp (-1)
  continue s
handleEvent s (AppEvent (Tick annotedAnalysis)) = do
  let nS =
        s
          { ticks = (s & ticks) + 1,
            analysis = annotedAnalysis
          }
  continue nS
handleEvent s _ = continue s

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (boolFalseAttr, fg V.red),
      (boolTrueAttr, fg V.green)
    ]

getInitialState :: MonadIO m => m AppState
getInitialState = do
  analysis <- loadAllPairAnalysis
  pure $ AppState analysis 0

main :: IO ()
main = do
  chan <- newBChan 10
  initialState <- getInitialState
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void
    . forkIO
    . forever
    $ do
      analysis <- loadAllPairAnalysis
      writeBChan chan $ Tick analysis
      threadDelay 100000
  void $ customMain initialVty buildVty (Just chan) app initialState
