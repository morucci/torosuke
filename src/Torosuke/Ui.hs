module Torosuke.Ui where

import Brick
import Brick.BChan
import Control.Concurrent (forkIO, threadDelay)
import qualified Graphics.Vty as V
import Relude
import Torosuke.Store (loadAllPairAnalysis)
import Torosuke.Types (AnnotatedAnalysis)

data Tick = Tick

data Name = Viewport1 deriving (Eq, Ord, Show)

data AppState = AppState
  { analysis :: [AnnotatedAnalysis],
    ticks :: Int
  }

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
      vBox $ analysisToWidget <$> analysis s
  ]
  where
    analysisToWidget :: AnnotatedAnalysis -> Widget Name
    analysisToWidget ana = str . fst $ fst ana

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
handleEvent s (AppEvent Tick) = do
  let nS = s {ticks = (s & ticks) + 1}
  continue nS
handleEvent s _ = continue s

theMap :: AttrMap
theMap = attrMap V.defAttr []

main :: IO ()
main = do
  analysis <- loadAllPairAnalysis
  let initialState = AppState analysis 0
      buildVty = V.mkVty V.defaultConfig
  chan <- newBChan 10
  initialVty <- buildVty
  void
    . forkIO
    . forever
    $ do
      writeBChan chan Tick
      threadDelay 100000
  void $ customMain initialVty buildVty (Just chan) app initialState
