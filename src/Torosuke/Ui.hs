module Torosuke.Ui where

import Brick
import qualified Graphics.Vty as V
import Relude

ui :: Text -> Widget ()
ui = str . toString

data AppState = AppState
  { items :: [Text],
    otherItems :: [Text]
  }

app :: App AppState e ()
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

drawUI :: AppState -> [Widget ()]
drawUI s = [hBox $ ui <$> items s]

handleEvent :: AppState -> BrickEvent () e -> EventM () (Next AppState)
handleEvent s (VtyEvent (V.EvKey V.KEsc [])) = halt s
handleEvent s _ = continue s

theMap :: AttrMap
theMap = attrMap V.defAttr []

main :: IO ()
main = do
  let initialState = AppState ["Hello", "Word", "!"] []
  void $ defaultMain app initialState
