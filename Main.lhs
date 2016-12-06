> module Main where
> import qualified HedTypes as HT
> import qualified TextObj as TO
> import qualified Event as E
> import qualified UI as UI

> import Data.Default (def)
> import Brick

> app :: App HT.TextObj () HT.Name
> app = App
>   { appDraw = UI.draw
>   , appChooseCursor = showFirstCursor
>   , appHandleEvent = E.handle
>   , appStartEvent = return
>   , appAttrMap = def
>   }

> main = defaultMain app (TO.emptyTO)
