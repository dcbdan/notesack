module Notesack.Misc ( getDate, (.>), (|>) ) where

import Data.Time.Clock
import Data.Time.Calendar

getDate :: IO String
getDate = do 
  x@(year,month,day) <- getCurrentTime >>= return . toGregorian . utctDay
  return $ (show year) ++ toStr month ++ toStr day
    where toStr i = let s = show i
                     in if length s == 1
                           then '0':s
                           else s

(.>) = flip (.)
(|>) = flip ($)
