module Torosuke.Ui where

import Brick
import Brick.BChan
import Control.Concurrent (forkIO, threadDelay)
import Data.Time.Format
import qualified Graphics.Vty as V
import Numeric
import Relude
import Torosuke.Store (loadAllPairAnalysis)
import Torosuke.Types (Analysis (aCloseT, aMacd, aMacdAnalisys), AnnotatedAnalysis, Macd (macdLine, signalLine), MacdAnalysis (maMVASL, maSLAZ))

newtype Tick = Tick [AnnotatedAnalysis]

data Name = Viewport1 deriving (Eq, Ord, Show)

data AppState = AppState
  { analysis :: [AnnotatedAnalysis],
    ticks :: Int
  }

showFullPrecision :: Double -> String
showFullPrecision x = showFFloat (Just 10) x ""

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
drawUI s =
  [ viewport Viewport1 Vertical $
      vBox $ [header] <> (analysisToWidget <$> analysis s)
  ]
  where
    header :: Widget Name
    header =
      hBox
        [ str "Pair/Interval",
          str "\t",
          str "Last candle date",
          str "\t",
          str "MLASL (p-1, p)",
          str "\t",
          str "SLAZ (p-1, p)",
          str "\t",
          str "ML/SL Diff (p-1, p)"
        ]
    analysisToWidget :: AnnotatedAnalysis -> Widget Name
    analysisToWidget ana =
      hBox $
        [ str . fst $ fst ana,
          str "/",
          str . snd $ fst ana,
          str "\t",
          str $ formatTime defaultTimeLocale "%F %R" $ aCloseT $ snd ana,
          str "\t"
        ]
          <> ( macdAnalysisToWidget . aMacdAnalisys $
                 snd ana
             )
          <> [ str "\t"
             ]
          <> ( macdToWidget . aMacd $
                 snd ana
             )
      where
        macdAnalysisToWidget :: MacdAnalysis -> [Widget Name]
        macdAnalysisToWidget macdA =
          intersperse (str " ") (boolWidget <$> reverse (take 2 (maMVASL macdA)))
            <> [str "\t"]
            <> intersperse (str " ") (boolWidget <$> reverse (take 2 (maSLAZ macdA)))
        macdToWidget :: Macd -> [Widget Name]
        macdToWidget macd = [str $ diffFullPrecision diff]
          where
            diff :: [Double]
            diff =
              uncurry (-)
                <$> zip
                  (reverse (take 2 (macdLine macd)))
                  (reverse (take 2 (signalLine macd)))
            diffFullPrecision :: [Double] -> String
            diffFullPrecision ld = toString . unwords $ toText . showFullPrecision <$> ld

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
