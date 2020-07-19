> {-# LANGUAGE OverloadedStrings #-}
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
> import Brick.Widgets.Center
> import qualified Graphics.Vty as V
> import qualified Yi.Rope as R
> import Control.DeepSeq
> import qualified Brick.AttrMap as A
> import qualified Brick.Focus as F


> -- |dispCursor function which will appropriately calculate the correct cursor
> --  position according to the current available width for the widget

> dispCursor ::n->(Int,Int)-> Widget n ->Widget n
> dispCursor n (grow,gcol) p =
>                  Widget Fixed Fixed $ do
>                          result <- render p
>                          let sz = (result ^.imageL.to V.imageWidth,result ^.imageL.to V.imageHeight-2)
>                          return $ result & cursorsL %~ (CursorLocation (Location (f (grow,gcol) sz)) (Just n):)
>  where
>   f (gr,gc) (ac,ar) |ac== 0 = def
>                     |otherwise = (rem gc ac,div gc ac)

> -- |adjustLength n ls will break ls into chunks with length n if smaller
> --  it will append spaces useful in softbreaking 

> adjustLength :: Int -> R.YiString -> [R.YiString]
> adjustLength numCols yis =case R.length yis <= numCols of
>   True -> [yis <> (R.replicateChar (numCols -(R.length yis)) ' ')]
>   _ -> (R.take numCols yis):(adjustLength numCols (R.drop numCols yis))

> -- |Renders a YiString after breaking it in lines and then also
> --  dividing according to the available width for the widget

> yistrToWidget::R.YiString -> Widget n
> yistrToWidget s =
>  Widget Fixed Fixed $ do
>       cont <- getContext
>       let lineImg lStr = V.string (cont^.attrL) lStr
>           coll = availWidth cont
>           ss = concat $ adjustLength coll <$> R.lines  s
>       return $ emptyResult & imageL .~ (V.vertCat (lineImg.R.toString <$> ss))

> drawTextObj::Bool->HT.TextObj-> Widget HT.Name
> drawTextObj foc s = fattr ed
>  where
>   [a,c,b] =[HT.above s, HT.leftOfC s <> HT.rightOfC s,HT.below s]
>   ed = viewport n Vertical $
>        vBox$zipWith ($) [id,(if foc then (dispCursor n (row,col)) else id).visible,id] $ yistrToWidget <$> [a,c,b]
>   (row,col) = TO.cursorPosition s
>   n = s ^. HT.nameL
>   fattr = if foc then withAttr "focussed" else id

> drawEditorObj::HT.EditorObj -> [Widget HT.Name]
> drawEditorObj eo = [ui]
>  where
>   ui = vBox[edpad,edstatus,edcomLine]
>   edpad = withAttr "edit" $drawTextObj (currFocus == (Just HT.EditPad)) (eo^.HT.editTextObjL)
>   edstatus = v$hCenterWith (Just '*').withAttr "status" $ str ((eo ^. HT.infoL) <> "   HED ~ The Haskell Editor   " <> (show.TO.cursorPosition$eo^.HT.editTextObjL))
>   edcomLine= v$withAttr "command" $ drawTextObj (currFocus== (Just HT.CommandPad)) (eo^.HT.commandObjL)
>   v = vLimit 1 . visible
>   currFocus = F.focusGetCurrent$ eo ^. HT.focusRingL


> -- | Map of Attributes to be used when rendering
> theMap::A.AttrMap
> theMap = A.attrMap V.defAttr 
>      [("edit", V.white `on` V.blue)
>      ,("status", flip V.withStyle V.bold $V.white `on` V.black)
>      ,("command",V.white `on` V.red)
>      ,("focussed",V.white `on` V.blue)]

> -- | convenience function to show cursor which currently have focus
> --   among all the resources
> appCursor::HT.EditorObj -> [CursorLocation HT.Name] -> Maybe (CursorLocation HT.Name)
> appCursor  = F.focusRingCursor (HT.focusRing)
