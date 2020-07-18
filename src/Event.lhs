> module Event where
> import qualified HedTypes as HT
> import qualified TextObj as TO
> import qualified CommandInterpreter as CI
> import qualified Commands as C

> import Brick
> import Data.Monoid
> import Control.Lens
> import Graphics.Vty
> import qualified Brick.Focus as F
> import Control.Monad

> handle:: HT.TextObj -> BrickEvent t t1 -> EventM n (Next HT.TextObj)
> handle st (VtyEvent (EvKey (KChar 'q') [])) = halt st
> handle st (VtyEvent e) = continue $ (applyEdit e) st
>
> applyEdit e = case e of
>           EvKey (KChar 'a') [MCtrl] -> TO.gotoBOL
>           EvKey (KChar 'e') [MCtrl] -> TO.gotoEOL
>           EvKey (KChar 'd') [MCtrl] -> TO.deleteChar
>           EvKey (KChar 'k') [MCtrl] -> TO.killToEOL
>           EvKey (KChar 'u') [MCtrl] -> TO.killToBOL
>           EvKey (KChar 'w') [MCtrl] -> TO.killToBOW
>           EvKey (KChar 'w') [MMeta] -> TO.killToEOW
>           EvKey (KChar 'z') [MCtrl] -> TO.deleteCurrLine
>           EvKey (KChar 'z') [MMeta] -> TO.killCurrWord
>           EvKey (KChar 'n') [MCtrl] -> TO.insNewLine
>           EvKey (KChar 'p') [MCtrl] -> snd.TO.delNextnChars 9
>           EvKey KEnter [] -> TO.breakLine
>           EvKey KDel [] -> TO.deleteChar
>           EvKey (KChar c) [] | c /= '\t' -> TO.insertChar c
>           EvKey KUp [] -> TO.moveUp
>           EvKey KDown [] -> TO.moveDown
>           EvKey KLeft [] -> TO.moveLeft
>           EvKey KRight [] -> TO.moveRight
>           EvKey KRight [MCtrl] -> TO.gotoEOW
>           EvKey KLeft [MCtrl] -> TO.gotoBOW
>           EvKey KBS [] -> TO.deletePrevChar
>           _ -> id

> applyEditorCommand::Event->HT.EditorObj->HT.EditorObj
> applyEditorCommand e = case e of
>           EvKey (KChar 'c') [MMeta] -> C.copyCurrentLineDefBuf
>           EvKey (KChar 'c') [MCtrl] -> C.copyCurrentWordDefBuf
>           EvKey (KChar 'e') [MMeta] -> C.copyTillEOL
>           EvKey (KChar 'a') [MMeta] -> C.copyTillBOL
>           EvKey (KChar 'v') [MCtrl] -> C.insBeforeCur
>           EvKey (KChar 'v') [MMeta] -> C.insAfterCur
>           EvKey (KChar 'v') [MMeta,MCtrl] -> C.insInNewLineBeforeCurr
>           _-> HT.editTextObjL %~ (applyEdit e)
>

> handleEditor::HT.EditorObj -> BrickEvent t t1 -> EventM n (Next HT.EditorObj)
> handleEditor st (VtyEvent (EvKey (KChar 'q') [MCtrl])) = halt st
> handleEditor st (VtyEvent (EvKey (KChar 'q') [MMeta])) = do  suspendAndResume (C.saveFile st) `seq` (return st)
>                                                              halt st
> handleEditor st (VtyEvent (EvKey (KChar 's') [MCtrl])) = suspendAndResume (C.saveFile st)
> handleEditor st (VtyEvent (EvKey  (KChar '\t') [])) = continue $ st & HT.focusRingL %~ F.focusNext
> handleEditor st (VtyEvent e) = case (F.focusGetCurrent $ st ^. HT.focusRingL) of 
>  Just HT.EditPad -> continue $ st & applyEditorCommand e
>  Just HT.CommandPad -> continue $ case e of 
>                                    (EvKey KEnter []) -> let s =(st & (HT.commandObjL %~ (applyEdit e)))
>                                                         in s & CI.evalCommand st
>                                    _ -> st & (HT.commandObjL %~ (applyEdit e))
>  _ {-Just HT.NoName-} -> continue st
>
