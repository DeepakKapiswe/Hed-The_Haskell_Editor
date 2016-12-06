> module Event where
> import qualified HedTypes as HT
> import qualified TextObj as TO

> import Brick
> import Data.Monoid
> import Control.Lens
> import Graphics.Vty

> handle:: HT.TextObj -> BrickEvent t t1 -> EventM n (Next HT.TextObj)
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

