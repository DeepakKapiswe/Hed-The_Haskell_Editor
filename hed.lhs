> import Data.Default (def)
> import Brick
> import Data.Monoid
> import Data.Char
> import Control.Lens
> import Control.Monad
> import Control.Monad.Trans
> import Brick.Widgets.Border
> import Graphics.Vty
> import qualified TextObj as TO

> type S = TO

> draw::S-> [Widget n]
> draw s = []

> app :: App S () String
> app = App
>   { appDraw = draw
>   , appChooseCursor = neverShowCursor
>   , appHandleEvent = handle
>   , appStartEvent = return
>   , appAttrMap = def
>   }

> handle s (VtyEvent (EvKey (KChar 'q') [])) = halt s
> handle s (VtyEvent e) =
>  let f = case e of
>           EvKey (KChar 'a') [MCtrl] -> TO.gotoBOL
>           EvKey (KChar 'e') [MCtrl] -> TO.gotoEOL
>           EvKey (KChar 'd') [MCtrl] -> TO.deleteChar
>           EvKey (KChar 'k') [MCtrl] -> TO.killToEOL
>           EvKey (KChar 'u') [MCtrl] -> TO.killToBOL
>           EvKey KEnter [] -> TO.breakLine
>           EvKey KDel [] -> TO.deleteChar
>           EvKey (KChar c) [] | c /= '\t' -> TO.insertChar c
>           EvKey KUp [] -> TO.moveUp
>           EvKey KDown [] -> TO.moveDown
>           EvKey KLeft [] -> TO.moveLeft
>           EvKey KRight [] -> TO.moveRight
>           EvKey KBS [] -> TO.deletePrevChar
>           _ -> id
>  in continue $ f s

> main = defaultMain app (TO.emptyTO)
