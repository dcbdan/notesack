module Notesack.EditStr ( 
  EditStr, snapCursor, isValidCursor, insert, delete, toLines, toText, fromText
) where 

import Data.Sequence ( Seq, fromList, (<|), (|>), (><) )
import qualified Data.Sequence as Seq

import Data.Foldable ( toList )

-- Here is my data structure to store rows of text for my text editor.
-- The "line number" is what line in a nx width text box a character might appear.
-- A "row number" is the line number if the text box was infinitely wide.

--               nx: the width of the box
--                   items: each row is a line,
--                          each line contains the row number it belongs on 
data EditStr = E Int (Seq (Int, Seq Char))

-- Where should the cursor be, if it is to enter text?
snapCursor :: EditStr -> (Int, Int) -> (Int, Int)
snapCursor (E nx lines) (x,y) = 
  if y < Seq.length lines 
     then let line = snd $ Seq.index lines y
              nl = length line
           in case (x < nl, nx == nl) of
                   -- x is on a character
                   (True, _)     -> (x,y)
                   -- x at the end of the line
                   (False,False) -> (nl,y)
                   -- x should be at the end of the line, but there is
                   -- no room..
                   (False,True)  -> (nl,y)--(0,y+1)
                   -- ^ note |iiii| <- snap on the last, have to have a new line.
                   --                  but ignoring for now
     else (0, Seq.length lines)

isValidCursor :: EditStr -> (Int, Int) -> Bool
isValidCursor e cursor = snapCursor e cursor == cursor

-- -- If cursorPos /= snapCursor cursorPos, then throw an error
-- -- Put a character at curosrPos, return the location of the 
insert :: EditStr -> (Int, Int) -> Char -> ((Int, Int), EditStr)
insert e cursor _ | not (isValidCursor e cursor) = error "invalid cursor" 

-- insert a new line
insert (E nx items) (x,y) '\n' = (newCursor, E nx newItems)
  where newCursor = (0,y+1)
        newItems = 
          case (y == Seq.length items, y == 0) of
            (True, True)  -> 
              Seq.insertAt y (0, Seq.fromList "") Seq.empty
            (True, False) -> 
              Seq.insertAt y (fst (Seq.index items (y-1)),Seq.fromList "") (incrementRow items)
            (False, _)    -> 
              let (lhs,rhs) = Seq.splitAt y items
                  (rowNum, line) = Seq.index rhs 0
                  (l,r) = Seq.splitAt x line
               in lhs >< ( (rowNum, l) <| (rowNum+1, r) <| incrementRow (Seq.drop 1 rhs))

insert (E nx items) (x,y) c = (newCursor, E nx newItems)          
  where newCursor = if x == nx - 1
                       then (0,y+1)
                       else (x+1,y)
        newItems = 
          case (y == Seq.length items, y == 0) of
            (True, True) -> 
              Seq.insertAt y (0, Seq.fromList [c]) items
            (True, False) ->
              Seq.insertAt y (fst (Seq.index items (y-1)),Seq.fromList [c]) items
            (False, _) ->
              let (lhs, rhs) = Seq.splitAt y items 
                  (rowNum, line) = Seq.index rhs 0
                  newLineFull = Seq.insertAt x c line
                  (newLine, leftOver) = Seq.splitAt nx newLineFull
               in lhs >< ((rowNum, newLine) <| recurse nx rowNum leftOver (Seq.drop 1 rhs))

recurse :: Int -> Int -> Seq Char -> Seq (Int, Seq Char) -> Seq (Int, Seq Char)
recurse nx n toAdd rest | null toAdd = rest
recurse nx n toAdd rest | null rest =
  if length toAdd <= nx
     then (n,toAdd) <| rest
     else let (lhs, rhs) = Seq.splitAt nx toAdd
           in (n,lhs) <| recurse nx n rhs rest
recurse nx n toAdd rest =
  let (m, line) = Seq.index rest 0
   in if n == m
         then let newLineFull = toAdd >< line
                  (lhs,rhs) = Seq.splitAt nx newLineFull
               in (n,lhs) <| recurse nx n rhs (Seq.drop 1 rest)
         else recurse nx n toAdd (Seq.empty) >< rest

incrementRow :: Seq (Int, Seq Char) -> Seq (Int, Seq Char)
incrementRow = fmap f
  where f (rowNum,x) = (rowNum+1,x)

decrementRow :: Seq (Int, Seq Char) -> Seq (Int, Seq Char)
decrementRow = fmap f
  where f (rowNum,x) = (rowNum-1,x)

delete :: EditStr -> (Int, Int) -> ((Int, Int), EditStr)
delete e cursor | not (isValidCursor e cursor) = error "invalid cursor" 
-- can't delete if off
delete e@(E _ items) cursor@(_,y) | y == length items = (cursor, e)
delete (E nx items) (x,y) | y == length items - 1 = ((x,y), E nx newItems)
  where (rowNum, line) = Seq.index items y
        newLine = if x == length line
                     then line
                     else Seq.deleteAt x line
        newItems = Seq.update y (rowNum, newLine) items
delete (E nx items) (x,y) = ((x,y), E nx newItems)
  where (lhs, rhs) = Seq.splitAt y items
        (rowNum, lineIn) = Seq.index rhs 0
        (newRowNum, nextLine) = Seq.index rhs 1
        (newLine,toAdd,rest) = 
          case (x == length lineIn, rowNum == newRowNum) of
            (False, False) -> 
              -- delete the item in the line, don't pull in the next line
              (Seq.deleteAt x lineIn, Seq.empty, Seq.drop 1 rhs)
            (False, True)  -> 
              -- delete the item in the line, pull in the next line
              let fullLine = Seq.deleteAt x lineIn >< nextLine
                  (newLine, toAdd) = Seq.splitAt nx fullLine
               in (newLine, toAdd, Seq.drop 2 rhs)
            (True, False)  ->
              let -- attach the next line
                  fullLine = Seq.deleteAt x lineIn >< nextLine
                  (newLine, toAdd) = Seq.splitAt nx fullLine
               in (newLine, toAdd, decrementRow (Seq.drop 2 rhs))
            (True, True)  ->
              -- no new line to attach, nothing to do
              (lineIn, Seq.empty, Seq.drop 1 rhs)
        newItems = lhs >< ((rowNum, newLine) <| recurse nx rowNum toAdd rest)

backspace :: EditStr -> (Int, Int) -> ((Int, Int), EditStr)
backspace = error "Notesack.EditStr.backspace: not implemented"

toLines :: EditStr -> [String]
toLines (E _ items) = recurse $ toList items
  where recurse [] = []
        recurse ((_,line):rest) = toList line : recurse rest

toText :: EditStr ->  String
toText (E _ items) = recurse 0 $ toList items
  where recurse _ [] = ""
        recurse i ((rowNumber, line):rest) = maybeNewLine ++ toList line ++ theRest
          where (maybeNewLine,theRest) = 
                  if i == rowNumber
                     then ("",   recurse i     rest)
                     else ("\n", recurse (i+1) rest)

-- String -> [(row number, String)] -> [(row number, [line])] -> [(row number, line)]
--           asLines                   asSplitLines              asOut
fromText :: Int -> String -> EditStr
fromText nx text = E nx items
  where asLines = zip [0..] $ linesMod text
        withSplitLines = map splitSnd asLines
        asOut = concat $ map addRowNumber $ withSplitLines
        items = fromList $ map toSeq $ asOut
        
        splitIt [] = []
        splitIt xs = (take nx xs) : splitIt (drop nx xs)
        splitSnd (n,xs) = (n, splitIt xs)

        addRowNumber (rowNumber, []) = [(rowNumber, [])]
        addRowNumber (rowNumber, xs) = map (\x -> (rowNumber, x)) xs
        toSeq (rowNumber, line) = (rowNumber, fromList line)

linesMod :: String -> [String]
linesMod ""   = []
linesMod xs = 
  let (lhs,rhs) = break (== '\n') xs
   in if null rhs
         then lhs:[]
         else lhs:linesMod (tail rhs) 

