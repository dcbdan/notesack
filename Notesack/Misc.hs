module Notesack.Misc ( getDate, isDateLike, (.>), (|>) ) where

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Char ( isDigit )

getDate :: IO String
getDate = do
  timeZone <- getCurrentTimeZone
  (year,month,day) <- getCurrentTime >>= 
                        (return . toGregorian . localDay . utcToLocalTime timeZone)
  return $ (show year) ++ toStr month ++ toStr day
    where toStr i = let s = show i
                     in if length s == 1
                           then '0':s
                           else s
  
--getDate :: IO String
--getDate = do 
--  x@(year,month,day) <- getCurrentTime >>= return . toGregorian . utctDay
--  return $ (show year) ++ toStr month ++ toStr day
--    where toStr i = let s = show i
--                     in if length s == 1
--                           then '0':s
--                           else s

isDateLike :: String -> Bool
isDateLike tag = length tag == 8 && all isDigit tag

(.>) = flip (.)
(|>) = flip ($)
