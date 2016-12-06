> {-# Language TemplateHaskell #-}
> module HedTypes where

> import Data.Default (def)
> import Data.Monoid
> import qualified Yi.Rope as R
> import SuffixLenses (suffixLenses)
>
>
> data Name = EditPad 
>           | CommandPad
>           | StatusPad deriving (Ord,Show,Eq)

> data TextObj = TO { above::R.YiString
>                   , leftOfC::R.YiString
>                   , rightOfC::R.YiString
>                   , below::R.YiString
>                   } deriving (Eq,Show)

> suffixLenses ''TextObj
