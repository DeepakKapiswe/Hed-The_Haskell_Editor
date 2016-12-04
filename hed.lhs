import Data.Default (def)
import Brick
import Data.Monoid
import Data.Char
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Brick.Widgets.Border
import Graphics.Vty
import qualified Data.Text.Zipper as Z

type S = Z.TextZipper String

draw::S-> [Widget n]
draw s = [(vBox.map str$Z.getText s)]{-vBox <$> [(vBox$str <$> ab),(vBox$str <$> dw)]
 where
  ab = Data.TextZipper.above s
  dw = below s  -}

cw :: String -> Widget n
cw s = Widget Fixed Fixed $ do
       ctx<- getContext
       render $ str (s <> " " <> show (ctx ^.availWidthL))

app :: App S () String
app = App
  { appDraw = draw
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handle
  , appStartEvent = return
  , appAttrMap = def
  }
{-
outOf::S-> IO S
outOf s = do
 putStrLn "out of event Loop" 
 ss <- getLine
 return $ss:s ++ [ss]
-}
handle s (VtyEvent (EvKey (KChar 'q') [])) = halt s
handle s (VtyEvent e) =
 let f = case e of
          EvKey (KChar 'a') [MCtrl] -> Z.gotoBOL
          EvKey (KChar 'e') [MCtrl] -> Z.gotoEOL
          EvKey (KChar 'd') [MCtrl] -> Z.deleteChar
          EvKey (KChar 'k') [MCtrl] -> Z.killToEOL
          EvKey (KChar 'u') [MCtrl] -> Z.killToBOL
          EvKey KEnter [] -> Z.breakLine
          EvKey KDel [] -> Z.deleteChar
          EvKey (KChar c) [] | c /= '\t' -> Z.insertChar c
          EvKey KUp [] -> Z.moveUp
          EvKey KDown [] -> Z.moveDown
          EvKey KLeft [] -> Z.moveLeft
          EvKey KRight [] -> Z.moveRight
          EvKey KBS [] -> Z.deletePrevChar
          _ -> id
 in continue $ f s
{-handle s (VtyEvent (EvKey (KChar 'q') [])) = halt (reverse s)
handle s (VtyEvent (EvKey (KChar '1') [])) =  suspendAndResume (outOf s)
handle s (VtyEvent (EvKey c [])) = 
 case c of 
  KEnter -> continue ("\n":s)
  KChar c | s/= [] ->  continue ( (head s ++[c]): (tail s))
  KChar c ->  continue ( [[c]])
handle s e = continue s
-}

main = defaultMain app (Z.stringZipper [] Nothing)
