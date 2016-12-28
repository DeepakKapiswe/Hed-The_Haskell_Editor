> {-# Language TemplateHaskell #-}
>
> module EditorObj where

> import HedTypes
> import TextObj
> import Buffer
> import Lens.Micro   ((^.), (&), (.~), (%~))
> import Data.Monoid
> import Data.Default (def)
> import Data.Char    (isPrint)
> import qualified Data.Text as T
> import qualified Yi.Rope as R
> import qualified Brick.Focus as F

> emptyEO = EO (F.focusRing [NoName,CommandPad,EditPad]) (emptyTO & nameL.~ EditPad) (emptyTO&nameL .~ CommandPad) emptyTO "NEW FILE" [] emptyBuf


> -- | goto Location moves the cursor of editTextObj to the specified 
> --   location if possible else most nearby possible location
>
> gotoLocation::(Int,Int)->EditorObj -> EditorObj
> gotoLocation (r,c) e = e & (editTextObjL %~ (moveCursor (r,c)))



