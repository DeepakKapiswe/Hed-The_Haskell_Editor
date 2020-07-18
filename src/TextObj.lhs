> {-# Language TemplateHaskell #-}
> {-# Language LambdaCase #-}

> module TextObj where

> import HedTypes
> import qualified Yi.Rope as R
> import Lens.Micro   ((^.), (&), (.~), (%~))
> import Data.Monoid
> import Data.Maybe   (fromMaybe)
> import Data.Char   (isPrint,isSpace,isControl)
> import qualified Data.Text as T

> emptyTO::TextObj
> emptyTO = TO mempty mempty mempty mempty NoName

> -- | creates a TextObj from the given String 
> makeTO::String->TextObj
> makeTO s = emptyTO & (rightOfCL .~r).(belowL .~b)
>  where y = R.fromString s
>        (r,b) = R.splitAtLine 1 y

> -- | Gives the text from the  Textobj in Text form
> getText::TextObj -> T.Text
> getText = R.toText.getYiString

> -- | Gives the text from the  Textobj in YiString form
> getYiString::TextObj -> R.YiString
> getYiString = mconcat .([above,leftOfC,rightOfC,below] <*>). pure


> -- | Gives Current Cursor Postion from the TextObj as (row,col)
> cursorPosition::TextObj -> (Int,Int)
> cursorPosition t = (R.countNewLines $ t ^. aboveL, R.length $ t ^. leftOfCL )

> -- | Checks whether the TextObj is at Top
> isTopLine::TextObj -> Bool
> isTopLine = R.null.above

> -- | Checks whether the TextObj is at Bottom
> isBotLine::TextObj -> Bool
> isBotLine = R.null.below

-----------------------------------------------------------------------------------------

> -- |cursor movement functions
> moveLeft :: TextObj -> TextObj
> moveLeft tObj | left == mempty = tObj
>               | otherwise = let (left',r') = R.splitAt (col-1) left
>                                 right' = r' <> (tObj ^. rightOfCL)
>                             in  tObj & (leftOfCL .~ left').(rightOfCL .~ right')
>  where col = R.length left
>        left = (tObj ^.leftOfCL)

> moveRight :: TextObj -> TextObj
> moveRight tObj | right == mempty || R.head right == Just ('\n') = tObj
>                | otherwise = let (l',right') = R.splitAt 1 right
>                                  left' = (tObj ^. leftOfCL) <> l'
>                              in  tObj & (leftOfCL .~ left').(rightOfCL .~ right')
>  where right = (tObj ^.rightOfCL)

> moveUp :: TextObj -> TextObj
> moveUp tObj | row ==0   = tObj
>             | otherwise = let (above',x) = R.splitAtLine (row-1) (tObj ^.aboveL)
>                               (left',right') = R.splitAt (if col < R.length x then col else (R.length x -1)) x
>                               below' = (tObj ^.leftOfCL) <> (tObj ^. rightOfCL) <> (tObj ^. belowL)
>                           in  tObj & (belowL .~ below').(aboveL .~ above').(leftOfCL .~ left').(rightOfCL .~ right')
>  where (row,col) = cursorPosition tObj

> moveDown :: TextObj -> TextObj
> moveDown tObj | isBotLine tObj = tObj
>               | otherwise = let (x,below') = R.splitAtLine 1 (tObj ^.belowL)
>                                 (left',right') = R.splitAt (if col < R.length x then col else (R.length x -1)) x
>                                 above' = (tObj ^.aboveL)<> (tObj ^.leftOfCL) <> (tObj ^. rightOfCL)
>                             in  tObj & (belowL .~ below').(aboveL .~ above').(leftOfCL .~ left').(rightOfCL .~ right')
>  where col = R.length $ (tObj ^. leftOfCL)

> -- | Move the cursor to the specified location if possible
> --   else moves to the most relavent postion
> moveCursor::(Int,Int) -> TextObj ->TextObj
> moveCursor (nRow,nCol) tObj =
>  let yiStr = getYiString tObj
>      (above',rest) = R.splitAtLine nRow yiStr
>      (curr,below') = R.splitAtLine 1 rest
>      c =if R.length curr > nCol then nCol else (R.length curr -1)
>      (left',right') = R.splitAt c curr
>  in tObj & (leftOfCL .~ left').(rightOfCL .~ right').(belowL .~ below').(aboveL .~ above')

-----------------------------------------------------------------------------------------

> currentLine::TextObj-> R.YiString
> currentLine t = (t ^.leftOfCL) <> (t ^.rightOfCL)

> breakLine::TextObj ->TextObj
> breakLine tObj = tObj & (aboveL %~ (<> ((tObj ^.leftOfCL) `R.snoc` '\n' )))
>                       & (leftOfCL .~ mempty)

> insertChar::Char->TextObj ->TextObj
> insertChar = \case
>  '\n' -> breakLine
>  x |isPrint x -> leftOfCL %~ (<> R.singleton x)
>  _->id

> -- | Insert many characters before the current cursor postion
> insertManyBefore::R.YiString ->TextObj ->TextObj
> insertManyBefore cp = leftOfCL %~ (<> R.filter (isPrint) cp)

> -- | Insert many characters after the current cursor postion
> insertManyAfter::R.YiString ->TextObj ->TextObj
> insertManyAfter cp = rightOfCL %~ (R.filter (isPrint) cp <>)

> -- | Insert many characters in the next line
> insertInNewLine::R.YiString ->TextObj->TextObj
> insertInNewLine c = insertManyBefore c .insNewLine

> insNewLine::TextObj -> TextObj
> insNewLine = breakLine.gotoEOL

> -- | Delete the character preceding the cursor position, and move the
> -- cursor backwards by one character.
> deletePrevChar ::TextObj -> TextObj
> deletePrevChar t =
>  case (lt==mempty) of
>    False -> deleteChar . moveLeft $ t
>    _ | isTopLine t -> t
>    _ -> let (above',l')  = R.splitAtLine (row-1) (t ^. aboveL)
>             l'' = maybe mempty id (R.init l')
>         in  t & (leftOfCL .~ l'').(aboveL .~ above')
>  where
>   lt = t ^. leftOfCL
>   row = R.countNewLines (t ^. aboveL)

> -- | Delete the character at the cursor position.  Leaves the cursor
> --   position unchanged.  If the cursor is at the end of a line of 
> --   textObj, this combines the line with the line below.
> deleteChar ::TextObj -> TextObj
> deleteChar t =
>    case (rt ==mempty) of
>       True  | R.null (t ^. belowL) -> t
>       False |rt==newLine && R.null (t ^. belowL) -> t & (rightOfCL .~ mempty)
>       False |rt==newLine  -> let (r',below')  = R.splitAtLine 1 (t ^. belowL)
>                   in t & (rightOfCL .~ r').(belowL .~ below')
>       _-> t & rightOfCL %~ (R.drop 1)
>  where
>   rt = t ^. rightOfCL
> newLine = R.singleton '\n'

> -- | delete the current line
> deleteCurrLine::TextObj ->TextObj
> deleteCurrLine = deleteChar.killToEOL.killToBOL

> -- | moves cursor to End of current Line
> gotoBOL::TextObj -> TextObj
> gotoBOL t= t & (leftOfCL .~ mempty).(rightOfCL %~ ( t ^. leftOfCL <>))

> -- | moves cursor to End of current Line
> gotoEOL::TextObj -> TextObj
> gotoEOL t= t & moveRight.moveLeft.(rightOfCL .~ mempty).(leftOfCL %~ (<> t ^. rightOfCL))

> -- | moves cursor to End of current Word or last
> --   space character if there are spaces
> gotoEOW::TextObj->TextObj
> gotoEOW t= let (l',r') = R.span (\x->(x/=' ') && (x/='\n')) (t^.rightOfCL)
>                (l'',r'') |l' /= mempty || r' == mempty || r'==newLine = (l',r')
>                          |otherwise = let (l''',r''') = R.span (\x->(x==' ') && (x/='\n')) r'
>                                       in (l'<>l''',r''')
>            in  t & (leftOfCL %~ (<> l'')).(rightOfCL .~ r'')

> -- | moves cursor to Begining of current Word or first
> --   space character if there are spaces
> gotoBOW::TextObj->TextObj
> gotoBOW t= let (r',l') = backspan (/=' ') (t^.leftOfCL)
>                (l'',r'') |r' /= mempty || l'== mempty = (l',r')
>                          |otherwise = let (r''',l''') = backspan (==' ') l'
>                                       in (l''',r'<>r''')
>            in  t & (leftOfCL .~  l'').(rightOfCL %~ (r''<>))
>  where backspan p t= (R.takeWhileEnd p t,R.dropWhileEnd p t)

> -- | Delete untill Begining of Line
> killToBOL::TextObj -> TextObj
> killToBOL = (leftOfCL .~ mempty)

> -- | Delete till End of Line
> killToEOL::TextObj -> TextObj
> killToEOL = (rightOfCL .~ newLine)

> -- | Delete till Begining of current Word or first
> --   space character if there are spaces
> killToBOW::TextObj -> TextObj
> killToBOW tObj = let l = tObj ^.leftOfCL
>                      l' | l==mempty = l
>                         | R.dropWhileEnd (/= ' ') l ==l = R.dropWhileEnd (== ' ') l
>                         | otherwise = R.dropWhileEnd (/= ' ') l
>                  in tObj & leftOfCL .~ l'

> -- | Delete till End of current Word or last
> --   space character if there are spaces
> killToEOW::TextObj -> TextObj
> killToEOW tObj = let r = tObj ^.rightOfCL
>                      r' | r==mempty || r==newLine = r
>                         | R.dropWhile (\x-> (x /= ' ') && (x /= '\n')) r ==r = R.dropWhile (\x-> (x == ' ') && (x /= '\n')) r
>                         | otherwise = R.dropWhile (\x->(x /= ' ') && (x /= '\n')) r
>                  in tObj & rightOfCL .~ r'


> -- | Delete current word at cursor
> killCurrWord::TextObj ->TextObj
> killCurrWord = killToBOW.killToEOW

> getNextnChars::Int->TextObj-> R.YiString
> getNextnChars n tObj = R.take n (tObj ^. rightOfCL <> tObj ^. belowL)

> getPrevnChars::Int->TextObj-> R.YiString
> getPrevnChars n tObj = R.take n (tObj ^. aboveL <> tObj ^. leftOfCL)

> delNextnChars::Int->TextObj -> (R.YiString,TextObj)
> delNextnChars n tObj = let (discarded,rest) = R.splitAt n (tObj ^. rightOfCL <> tObj ^. belowL)
>                            (right',below')  = R.splitAtLine 1 rest
>                        in  (discarded,tObj & (rightOfCL .~ right').(belowL .~ below'))

> delPrevnChars::Int->TextObj -> (R.YiString,TextObj)
> delPrevnChars n tObj = let (discarded,rest) = R.splitAt n (tObj ^. aboveL <> tObj ^. leftOfCL)
>                            n = R.countNewLines rest
>                            (above',left')  = R.splitAtLine (n-1) rest
>                        in  (discarded,tObj & (leftOfCL .~ left').(aboveL .~ above'))


> -- | Gives current word empty if cursor is at space
> getCurrWord::TextObj -> R.YiString
> getCurrWord t = let (l,r) = (R.takeWhileEnd p (t ^.leftOfCL),R.takeWhile p (t ^.rightOfCL))
>                     p  = \x->not (isSpace x ||isControl x)
>                 in l<>r

> getTillEOL::TextObj -> R.YiString
> getTillEOL = fromMaybe mempty .R.init.(^.rightOfCL)

> getTillBOL::TextObj -> R.YiString
> getTillBOL = (^.leftOfCL)

