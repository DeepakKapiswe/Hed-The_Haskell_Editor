> module Main where
> import qualified HedTypes as HT
> import qualified EditorObj as EO
> import qualified TextObj as TO
> import qualified Event as E
> import qualified UI as UI

> import Control.Lens
> import Control.Exception

> import Data.Default (def)
> import Brick

> app :: App HT.EditorObj () HT.Name
> app = App
>   { appDraw = UI.drawEditorObj
>   , appChooseCursor = UI.appCursor
>   , appHandleEvent = E.handleEditor
>   , appStartEvent = return
>   , appAttrMap = const UI.theMap
>   }

> hed f = do
>  file <-safeLoadFile f
>  let contents = case file of 
>         Left _ -> ""
>         Right x -> x
>  defaultMain app (EO.emptyEO & (HT.editTextObjL.HT.nameL .~ HT.EditPad).(HT.editTextObjL .~ (TO.makeTO contents)).(HT.infoL .~ f))




> safeLoadFile :: FilePath -> IO (Either IOException String)
> safeLoadFile f = (Right <$> readFile f) `catch` (\ e -> pure (Left e) )
