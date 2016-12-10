> module Main where
> import qualified HedTypes as HT
> import qualified EditorObj as EO
> import qualified TextObj as TO
> import qualified Event as E
> import qualified UI as UI

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

> main = defaultMain app (EO.emptyEO)
