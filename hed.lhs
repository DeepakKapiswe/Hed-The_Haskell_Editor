> import Data.Default (def)
> import Brick
> import Data.Monoid
> import Data.Char
> import Control.Lens
> import Control.Monad
> import Control.Monad.Trans
> import Brick.Widgets.Core
> import Graphics.Vty
> import qualified TextObj as TO
> import qualified Yi.Rope as R

now what is the current state of the editor

now i have made an interface through which i could represent the state of the actual text object in an abstract manner and also the required minimum functions to transform the textObj

now what are you thinking to do 

now i have to think that how can i display my text object to the terminal so that the user could modify it interctively, for that what i need is
 > show cursor at correct position
 > make the current line always visible i.e support scrolling vertically
 > i have to display such that the longer lines should be broken and incomplete lines should not be shown to the user they should be replaced with   vaccant lines while displaying



> data Name = EditPad 
>           | CommandPad
>           | StatusPad deriving (Ord,Show,Eq)

> type S = TO.TextObj

> draw::S-> [Widget Name]
> draw s = [ui]
>  where
>   [a,c,b] = txt.R.toText <$> [TO.above s, TO.leftOfC s <> TO.rightOfC s,TO.below s]
>   ui = viewport EditPad Vertical . vBox $ [a,visible c,b]

> myTextObjRenderer::S -> Widget n
> myTextObjRenderer s = undefined


> app :: App S () Name
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
