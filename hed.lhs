> import Data.Default (def)
> import Brick -- this is comment
> import Data.Monoid
> import Control.Lens
> import Control.Monad
> import Control.Monad.Reader
> import Control.Monad.State
> import Control.Monad.Trans
> import Brick.Widgets.Core
> import Graphics.Vty
> import qualified Graphics.Vty as V
> import qualified TextObj as TO
> import qualified Yi.Rope as R
> import Control.DeepSeq

4-12-2016

now what is the current state of the editor

now i have made an interface through which i could represent the state of the actual text object in an abstract manner and also the required minimum functions to transform the textObj

now what are you thinking to do 

now i have to think that how can i display my text object to the terminal so that the user could modify it interctively, for that what i need is
 1> show cursor at correct position --DONE
 2> make the current line always visible i.e support scrolling vertically -- DONE 
 3> i have to display such that the longer lines should be broken and incomplete lines should not be shown to the user they should be replaced with   vaccant lines while displaying --DONE


now i have done the 3rd above mentioned point but now a new porblem i am facing of cursor position misplacement and now i have to comeup with some formula to calculate correctly the cursor position

6-12-2016 03:30 pm
 now the problem has been solved of correctness of cusor positon i.e now I am able to display cusor at correct position also supporting soft line break
--------------------------------------------------------------------------------------------------------------------------------------------------

> myshowCursor ::n->(Int,Int)-> Widget n ->Widget n
> myshowCursor n (grow,gcol) p =
>                  Widget Fixed Fixed $ do
>                          result <- render p
>                          let sz = (result ^.imageL.to V.imageWidth,result ^.imageL.to V.imageHeight)
>                          return $ result & cursorsL %~ (CursorLocation (Location (f (grow,gcol) sz)) (Just n):)
>  where
>   f (gr,gc) (ac,ar) =case ac of
>                   0 -> (ac,ar)
>                   x -> (rem gc ac,div gc ac)

> adjustLength :: Int -> R.YiString -> [R.YiString]
> adjustLength numCols yis =case R.length yis <= numCols of
>   True -> [yis <> (R.replicateChar (numCols -(R.length yis)) ' ')]
>   _ -> (R.take numCols yis):(adjustLength numCols (R.drop numCols yis))

> data Name = EditPad 
>           | CommandPad
>           | StatusPad deriving (Ord,Show,Eq)

> type S = TO.TextObj


> myTextObjRenderer::R.YiString -> Widget n
> myTextObjRenderer s =
>  Widget Fixed Fixed $ do
>       cont <- getContext
>       let lineImg lStr = V.string (cont^.attrL) lStr
>           coll = availWidth cont
>           ss = concat $ adjustLength coll <$> R.lines  s
>       return $ def & imageL .~ (V.vertCat (lineImg.R.toString <$> ss))

> draw::S-> [Widget Name]
> draw s = [ed]
>  where
>   [a,c,b] =[TO.above s, TO.leftOfC s <> TO.rightOfC s,TO.below s]
>   ed = viewport EditPad Vertical $
>        vBox$zipWith ($) [id,myshowCursor EditPad (row,col) .visible,id] $ myTextObjRenderer <$> [a,c,b]
>   (row,col) = TO.cursorPosition s

> app :: App S () Name
> app = App
>   { appDraw = draw
>   , appChooseCursor = showFirstCursor
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
