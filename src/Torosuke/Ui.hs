module Torosuke.Ui where

import Brick
import Brick.BChan
import Control.Concurrent (forkIO, threadDelay)
import qualified Graphics.Vty as V
import Relude

ui :: Text -> Widget ()
ui = str . toString

data Tick = Tick

data AppState = AppState
  { items :: [Text],
    ticks :: Int
  }

app :: App AppState Tick ()
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

drawUI :: AppState -> [Widget ()]
drawUI s = [hBox $ ui <$> (items s <> [show $ s & ticks])]

handleEvent :: AppState -> BrickEvent () Tick -> EventM () (Next AppState)
handleEvent s (VtyEvent (V.EvKey V.KEsc [])) = halt s
handleEvent s (AppEvent Tick) = do
  let nS = s {ticks = (s & ticks) + 1}
  continue nS
handleEvent s _ = continue s

theMap :: AttrMap
theMap = attrMap V.defAttr []

main :: IO ()
main = do
  let initialState = AppState ["Hello", "Word", "!"] 0
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
