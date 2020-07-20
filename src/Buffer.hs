{-# Language TemplateHaskell #-}

module Buffer where

import HedTypes
import Lens.Micro   ((^.), (&), (.~), (%~))
import Data.Monoid
import Data.Default (def)
import qualified Yi.Rope as R

emptyBuf::Buffer R.YiString
emptyBuf = Buffer mempty mempty mempty mempty mempty

getDefBufContents::Buffer a -> a
getDefBufContents = (^. b1L)

setDefBufContents::a-> Buffer a -> Buffer a
setDefBufContents a = (& b1L .~ a)

setBufnContents::Int->a->Buffer a ->Buffer a
setBufnContents n a | n==1 = (& b1L.~ a)
                    | n==2 = (& b2L.~ a)
                    | n==3 = (& b3L.~ a)
                    | n==4 = (& b4L.~ a)
                    | n==5 = (& b5L.~ a)
                    | otherwise = id

getBufnContents::Int->Buffer a -> a
getBufnContents n | n==1 = (^. b1L)
                  | n==2 = (^. b2L)
                  | n==3 = (^. b3L)
                  | n==4 = (^. b4L)
                  | n==5 = (^. b5L)
                  | otherwise = (^.b1L)


clearAllBufferContents::Buffer R.YiString-> Buffer R.YiString
clearAllBufferContents = const emptyBuf

clearBufnContents::(Monoid a) => Int->Buffer a ->Buffer a
clearBufnContents n = setBufnContents n mempty

