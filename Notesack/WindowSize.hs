module Notesack.WindowSize (
  getWindowSize
) where

import System.Posix.Terminal
import System.Posix.IO (fdRead, stdInput)
import Control.Exception (finally, catch, IOException)

import System.IO ( hPutChar, hPutStr, hFlush, stdout )
import System.Process ( readProcess )

import Control.Monad

-- The console is interacted via hPut and hFlu
hPut = hPutStr stdout
hCha = hPutChar stdout
hFlu = hFlush stdout

-------------------------------------------------------------------------------
-- https://stackoverflow.com/questions/23068218/haskell-read-raw-keyboard-input
{- run an application in raw input / non-canonical mode with given
 - VMIN and VTIME settings. for a description of these, see:
 - http://www.gnu.org/software/libc/manual/html_node/Noncanonical-Input.html
 - as well as `man termios`.
 -}
withRawInput :: Int -> Int -> IO a -> IO a
withRawInput vmin vtime application = do

  {- retrieve current settings -}
  oldTermSettings <- getTerminalAttributes stdInput

  {- modify settings -}
  let newTermSettings = 
        flip withoutMode  EnableEcho   . -- don't echo keystrokes
        flip withoutMode  ProcessInput . -- turn on non-canonical mode
        flip withTime     vtime        . -- wait at most vtime decisecs per read
        flip withMinInput vmin         $ -- wait for >= vmin bytes per read
        oldTermSettings

  {- install new settings -}
  setTerminalAttributes stdInput newTermSettings Immediately

  {- restore old settings no matter what; this prevents the terminal
   - from becoming borked if the application halts with an exception
   -}
  application 
    `finally` setTerminalAttributes stdInput oldTermSettings Immediately

getWindowSize :: IO (Int, Int)
getWindowSize = do
  -- readProcess for tput returns <num>\n so remove that before converting to a number
  numX <- (read . init) <$> readProcess "tput" ["cols"] ""
  numY <- (read . init) <$> readProcess "tput" ["lines"] ""
  return (numX, numY)

-- -- TODO: Bug: sometimes read fails. (This happened when a lot of getWindowSize was being called)
-- getWindowSize ::IO (Int, Int)
-- getWindowSize = do
--   hPut saveCursor
--   hPut $ moveCursor 1000 3000
--   hPut (esc:"[6n")
--   hPut restoreCursor
--   hFlu
--   -- Need atleast 6 bytes (for your 1 x 1 terminals..)
--   -- Wait at most 0.1 seconds for the input
--   -- read up to 20 bytes (for your 10,000 x 10,000 terminals..)
--   w <- withRawInput 6 1 $ do
--     (str, bytes) <- fdRead stdInput 20
--     return (show str)
--   -- parse "\ESC[49;87R" into 49, 87
--   let (numY, strX) = span (/= ';') $ drop 1 $ dropWhile (/= '[') w
--       numX = takeWhile (/= 'R') $ drop 1 strX
--   return (read numX, read numY)

esc = '\x1b'
saveCursor = esc:"7"
restoreCursor = esc:"8"

moveCursor :: Int -> Int -> String
moveCursor x y = esc:"[" ++ show (y+1) ++ ";" ++ show (x+1) ++ "H"

