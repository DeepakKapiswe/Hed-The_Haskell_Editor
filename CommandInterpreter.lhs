> {-# Language TemplateHaskell #-}

> module CommandInterpreter where

> import qualified EditorObj as E
> import qualified TextObj as T
> import qualified Buffer as B
> import HedTypes

> import Lens.Micro   ((^.), (&), (.~), (%~))
> import qualified Yi.Rope as R

> type TransformEditor = EditorObj -> EditorObj


> interpretCommand::String->TransformEditor
> interpretCommand s = case words s of
>  ["goto",x,y] -> E.gotoLocation (read x::Int,read y::Int)
>  ["ins"] -> \x-> let y = B.getDefBufContents $ x ^. bufferL
>                  in x & E.insertText y
>  ["ins",bufn] -> \x-> let y = B.getBufnContents (read bufn::Int) $ x ^. bufferL
>                  in x & E.insertText y
>  ["yank"] -> \x-> let y = T.currentLine $ x ^. editTextObjL
>                   in x & bufferL %~ (B.setDefBufContents y)
>  ["yank",nChars] -> \x-> let y = T.getNextnChars  (read nChars::Int) $ x ^. editTextObjL
>                          in x & bufferL %~ (B.setDefBufContents y)
>  ["yank",nChars,bufn] -> \x-> let y = T.getNextnChars  (read nChars::Int) $ x ^. editTextObjL
>                               in x & bufferL %~ (B.setBufnContents (read bufn::Int) y)
>  ["deln",nChars,bufn] -> \x-> let (y,edObj) = T.delNextnChars  (read nChars::Int) $ x ^. editTextObjL
>                               in x & (bufferL %~ (B.setBufnContents (read bufn::Int) y)).(editTextObjL .~ edObj)
>  ["deln",nChars] -> \x-> let (y,edObj) = T.delNextnChars  (read nChars::Int) $ x ^. editTextObjL
>                          in x & (bufferL %~ (B.setDefBufContents  y)).(editTextObjL .~ edObj)
>  ["delp",nChars,bufn] -> \x-> let (y,edObj) = T.delPrevnChars  (read nChars::Int) $ x ^. editTextObjL
>                               in x & (bufferL %~ (B.setBufnContents (read bufn::Int) y)).(editTextObjL .~ edObj)
>  ["delp",nChars] -> \x-> let (y,edObj) = T.delPrevnChars  (read nChars::Int) $ x ^. editTextObjL
>                          in x & (bufferL %~ (B.setDefBufContents  y)).(editTextObjL .~ edObj)
>  _ -> id


> getCommandToInterpret::EditorObj->String
> getCommandToInterpret e = R.toString .T.currentLine $ (e ^. commandObjL)


> evalCommand::EditorObj -> TransformEditor
> evalCommand e = interpretCommand (s e)
>  where
>   s = getCommandToInterpret
