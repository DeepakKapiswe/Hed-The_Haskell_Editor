> module UI where
> import qualified HedTypes as HT
> import qualified TextObj as TO
> import qualified Event as E

> import Data.Default (def)
> import Brick 
> import Data.Monoid
> import Control.Lens
> import Control.Monad
> import Control.Monad.Reader
> import Control.Monad.State
> import Control.Monad.Trans
> import Brick.Widgets.Core
> import Graphics.Vty
> import qualified Graphics.Vty as V
> import qualified Yi.Rope as R
> import Control.DeepSeq


> type S = HT.TextObj


> myshowCursor ::n->(Int,Int)-> Widget n ->Widget n
> myshowCursor n (grow,gcol) p =
>                  Widget Fixed Fixed $ do
>                          result <- render p
>                          let sz = (result ^.imageL.to V.imageWidth,result ^.imageL.to V.imageHeight)
>                          return $ result & cursorsL %~ (CursorLocation (Location (f (grow,gcol) sz)) (Just n):)
>  where
>   f (gr,gc) (ac,ar) |ac== 0 = def
>                     |otherwise = (rem gc ac,div gc ac)

> adjustLength :: Int -> R.YiString -> [R.YiString]
> adjustLength numCols yis =case R.length yis <= numCols of
>   True -> [yis <> (R.replicateChar (numCols -(R.length yis)) ' ')]
>   _ -> (R.take numCols yis):(adjustLength numCols (R.drop numCols yis))


> myTextObjRenderer::R.YiString -> Widget n
> myTextObjRenderer s =
>  Widget Fixed Fixed $ do
>       cont <- getContext
>       let lineImg lStr = V.string (cont^.attrL) lStr
>           coll = availWidth cont
>           ss = concat $ adjustLength coll <$> R.lines  s
>       return $ def & imageL .~ (V.vertCat (lineImg.R.toString <$> ss))

> draw::S-> [Widget HT.Name]
> draw s = [ed]
>  where
>   [a,c,b] =[HT.above s, HT.leftOfC s <> HT.rightOfC s,HT.below s]
>   ed = viewport HT.EditPad Vertical $
>        vBox$zipWith ($) [id,myshowCursor HT.EditPad (row,col) .visible,id] $ myTextObjRenderer <$> [a,c,b]
>   (row,col) = TO.cursorPosition s
