module Notesack.EditStr ( 
  EditStr, snapCursor, insert, delete, backspace, toLines, toText, fromText
) where 

import Data.Sequence ( Seq, fromList, (<|), (|>), (><) )
import qualified Data.Sequence as Seq

import Data.Foldable ( toList )

-- TODO: change this api so that the EditStr contains the cursor position directly
--       (Also, the editstr should contain any meta data.. in wrapped line mode
--        which is not implemented, you have to know if the cursor is on a new line or
--        not...)
data EditStr = E (Seq (Seq Char))

snapCursor :: EditStr -> (Int, Int) -> (Int, Int)
snapCursor (E items) (x,y) =
  let nx = Seq.length (Seq.index items y)
      ny = Seq.length items 
   in case (y < ny, x < nx) of
        (True, True)  -> (x,y)
        (True, False) -> (nx,y)
        (False, _)    -> (0,ny)

isValidCursor :: EditStr -> (Int, Int) -> Bool
isValidCursor e cursor = snapCursor e cursor == cursor

fixItems y items = 
  if Seq.length items == y
     then items |> Seq.empty
     else items

insert :: EditStr -> (Int, Int) -> Char -> ((Int, Int), EditStr)
insert e cursor _ | not (isValidCursor e cursor) = error "invalid cursor" 
insert (E items) (x,y) '\n' = ((0,y+1), E newItems)
  where (lhs,rhs) = Seq.splitAt y $ fixItems y items
        line = Seq.index rhs 0
        (l,r) = if x < Seq.length line
                   then Seq.splitAt x line
                   else (line,Seq.empty)
        newItems = (lhs |> l) >< (r <| Seq.drop 1 rhs)

insert (E items) (x,y) c = ((x+1,y), E newItems)
  where (lhs,rhs) = Seq.splitAt y $ fixItems y items
        line = Seq.index rhs 0
        newLine = Seq.insertAt x c line
        newItems = lhs >< (newLine <| (Seq.drop 1 rhs)) 

delete :: EditStr -> (Int, Int) -> ((Int, Int), EditStr)
delete (E items) (x,y) = ((x,y), E newItems)
  where (lhs, rhs) = Seq.splitAt y $ fixItems y items
        line = Seq.index rhs 0
        rest = Seq.drop 1 rhs
        newItems = 
          if x == Seq.length line
             then let (restLine,rest') = 
                        if Seq.length rest == 0
                           then (Seq.empty, Seq.empty)
                           else (Seq.index rest 0, Seq.drop 1 rest)
                   in (lhs |> (line >< restLine)) >< rest'
             else let newLine = Seq.deleteAt x line
                   in lhs >< (newLine <| rest)
--        (newCursor, newLine) = 
--          if x == Seq.length line
--            then ((x-1,y), line)
--            else ((x,y), Seq.deleteAt x line)
--        newItems = lhs >< (newLine <| (Seq.drop 1 rhs))

backspace :: EditStr -> (Int, Int) -> ((Int, Int), EditStr)
backspace e (0,0) = ((0,0), e)
backspace (E items) (0,y) = (newCursor, E newItems)
  where (lhs, rhs) = Seq.splitAt (y-1) $ fixItems y items
        prevLine = Seq.index rhs 0
        line     = Seq.index rhs 1
        newCursor = (Seq.length prevLine, y-1)
        newItems = lhs >< ((prevLine >< line) <| (Seq.drop 2 rhs))
backspace (E items) (x,y) = ((x-1,y), E newItems)
  where (lhs, rhs) = Seq.splitAt y $ fixItems y items
        line = Seq.index rhs 0
        newLine = Seq.deleteAt (x-1) line
        newItems = lhs >< (newLine <| (Seq.drop 1 rhs))

toLines :: EditStr -> [String]
toLines = map toList . toList . unE

toText :: EditStr ->  String
toText = unlines . toLines

unE (E x) = x

fromText :: Int -> String -> EditStr
fromText _ = E . fromList . map fromList . linesMod

linesMod :: String -> [String]
linesMod ""   = []
linesMod xs = 
  let (lhs,rhs) = break (== '\n') xs
   in if null rhs
         then lhs:[]
         else lhs:linesMod (tail rhs) 

