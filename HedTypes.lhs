> {-# Language TemplateHaskell #-}
> {-# Language MultiParamTypeClasses #-}
> module HedTypes where

> import Data.Default (def)
> import Data.Monoid
> import qualified Yi.Rope as R
> import SuffixLenses (suffixLenses)
> import qualified Brick.Focus as F
> import Brick.Widgets.Core (Named(..))
>
>
> data Name = EditPad
>           | CommandPad
>           | StatusPad
>           | ListPad
>           | NoName deriving (Ord,Show,Eq)

> data TextObj = TO { above::R.YiString
>                   , leftOfC::R.YiString
>                   , rightOfC::R.YiString
>                   , below::R.YiString
>                   , name::Name
>                   } deriving (Eq,Show)
> instance Named TextObj Name where
>  getName = name


> data EditorObj = EO { focusRing::F.FocusRing Name
>                     , editTextObj::TextObj
>                     , commandObj::TextObj
>                     , otherObj::TextObj
>                     , info::String
>                     , msg :: [String]
>                     , buffer::[R.YiString]
>                     } --deriving (Eq,Show)

> suffixLenses ''TextObj
> suffixLenses ''EditorObj
