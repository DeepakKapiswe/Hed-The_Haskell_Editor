> {-# Language TemplateHaskell #-}
> {-# Language LambdaCase #-}

> module EditorObj where

> import HedTypes
> import TextObj

> import Lens.Micro   ((^.), (&), (.~), (%~))
> import Data.Monoid
> import Data.Default (def)
> import Data.Char    (isPrint)

> import qualified Data.Text as T
> import qualified Yi.Rope as R
> import qualified Brick.Focus as F

> emptyEO = EO (F.focusRing [NoName,CommandPad,EditPad]) (emptyTO & nameL.~ EditPad) (emptyTO&nameL .~ CommandPad) emptyTO "NEW FILE" [] [] 
>
