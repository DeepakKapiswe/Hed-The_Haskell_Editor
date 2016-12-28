> {-# Language TemplateHaskell #-}

> module Commands where

> import qualified EditorObj as E
> import qualified TextObj as T
> import qualified Buffer as B
> import HedTypes

> import Lens.Micro  -- ((^.), (&), (.~), (%~))
> import qualified Yi.Rope as R
> import Data.Maybe (fromMaybe)
> import Control.Monad.Trans


> --type TransformEditor = EditorObj -> EditorObj

> copyCurrentLineDefBuf::EditorObj -> EditorObj
> copyCurrentLineDefBuf x = let y = T.currentLine $ x ^. editTextObjL
>                           in x & bufferL %~ (B.setDefBufContents y)

> copyTillEOL::EditorObj->EditorObj
> copyTillEOL x = let y = T.getTillEOL $ x ^. editTextObjL
>                 in x & bufferL %~ (B.setDefBufContents y)

> copyTillBOL::EditorObj->EditorObj
> copyTillBOL x = let y = T.getTillBOL $ x ^. editTextObjL
>                 in x & bufferL %~ (B.setDefBufContents y)

> insBeforeCur::EditorObj->EditorObj
> insBeforeCur x = let y = B.getDefBufContents $ x ^. bufferL
>                  in x & editTextObjL %~ (T.insertManyBefore y)

> insAfterCur::EditorObj -> EditorObj
> insAfterCur x = let y = B.getDefBufContents $ x ^. bufferL
>                 in x & editTextObjL %~ (T.insertManyAfter y)

> insInNewLineBeforeCurr::EditorObj ->EditorObj
> insInNewLineBeforeCurr x = let y = B.getDefBufContents $ x^.bufferL
>                            in x & editTextObjL %~ (T.insertInNewLine y)

> saveFile::EditorObj-> IO EditorObj
> saveFile e = do
>  liftIO (writeFile (e^. infoL) (R.toString. T.getYiString $ (e ^.editTextObjL)))
>  return e

