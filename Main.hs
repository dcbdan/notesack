{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc ( free )

import Control.Monad.RWS
import Control.Monad.Except

import Graphics.Vty hiding ( Mode, setMode )
import Graphics.Vty.Picture
import Graphics.Vty.Image ( emptyImage, charFill )
import Graphics.Vty.Attributes ( defAttr, withBackColor )
import Graphics.Vty.Attributes.Color ( blue )

import System.Environment(getArgs)
import System.Directory

import Notesack.Types
import Notesack.Database
import Notesack.Misc 

main :: IO ()
main = do args <- getArgs
          vty <- mkVty defaultConfig
          maybeErr <- runExceptT (mainExcept vty args)
          shutdown vty
          case maybeErr of
            Left s  -> putStrLn s
            Right _ -> return ()

mainExcept vty [filename] = do
  today <- liftIO getDate
  notesackSetup filename
  loadView today
  let initEnv = Env vty SackConfig
      initState = State (TableView today (0,0) Nothing) (ModeSelect Nothing) (0,0)
  execRWST (sackInteract False) initEnv initState >> return ()  
  notesackShutdown

mainExcept _ _ = throwError "Usage: notesack FILE"

sackInteract :: Bool -> Sack ()
sackInteract shouldExit = do
  unless shouldExit $ handleNextEvent >>= sackInteract

-- TODO create a select region mode

handleNextEvent = askVty >>= liftIO . nextEvent >>= handleEvent
  where handleEvent (EvKey KEsc []) = return True
        handleEvent event = do
          mode <- getMode
          case mode of
            ModeSelect selectRegion -> handleModeSelect event selectRegion
          drawSack
          return False

handleModeSelect :: Event -> Maybe (Pos,Pos) -> Sack ()

handleModeSelect (EvKey (KChar ' ') []) Nothing = do
  cursor <- getCursor
  putMode (ModeSelect (Just cursor))

handleModeSelect (EvKey (KChar ' ') []) (Just _) = putMode (ModeSelect Nothing)

-- If we're selecting, make sure that expanding in the direction is 
-- not going to bump into another box.
handleModeSelect (EvKey (KChar c) []) (Just (x,y)) | c `elem` "hjkl" = 
  let dir = dirFromChar c
      selectRegion (xx,yy) =
        let (Box l r u d) = toBox (x,y) (xx,yy)
         in case dir of
              DirL -> Box (l-1) (l-1) u d
              DirR -> Box (r+1) (r+1) u d
              DirU -> Box l r (u-1) (u-1)
              DirD -> Box l r (d+1) (d+1)
  in do (xx,yy) <- getCursor
        let region = selectRegion (xx,yy)
        viewId <- getViewId
        canMove <- not <$> lift (areaHasNote viewId region)
        if canMove
           then moveCursor dir
           else return ()
        
-- Not selecting anything, so just move the cursor
handleModeSelect (EvKey (KChar c) []) Nothing | c `elem` "hjkl" = 
  let dir = dirFromChar c
   in moveCursor dir

handleModeSelect _ _ = return ()

dirFromChar 'h' = DirL
dirFromChar 'j' = DirD
dirFromChar 'k' = DirU
dirFromChar 'l' = DirR

drawSack :: Sack ()
drawSack = do
  vty <- askVty
  (x,y) <- getCursor
  ModeSelect maybeSelected <- getMode
  let (image, cursorObj) = 
        case maybeSelected of
                Nothing -> (emptyImage, AbsoluteCursor x y)
                Just (xx,yy) -> (selectImage (toBox (x,y) (xx,yy)), AbsoluteCursor x y)
  let picture = (picForImage image){ picCursor = cursorObj }
  liftIO $ update vty picture 

selectImage :: Box -> Image
selectImage (Box l r u d) = charFill attr ' ' (r-l+1) (d-u+1) |> translate l u
  where attr = withBackColor defAttr blue

toBox (x1,y1) (x2,y2) = Box (min x1 x2) (max x1 x2) (min y1 y2) (max y1 y2)

--  where handleEvent (EvKey (KChar 'a') []) = do
--          return False
--        handleEvent e = return $ e == EvKey KEsc []
--
--------------------------------------------------------------------------------------

unlessM bool v = do
  maybeSo <- bool
  unless maybeSo v

-- If we have the view, let it be the view,
-- otherwise, add the view
loadView :: String -> ExceptM ()
loadView viewId = 
  unlessM (hasView viewId) (addView (TableView viewId (0,0) Nothing))

notesackSetup :: String -> ExceptM ()
notesackSetup dbFile = do 
  isDir  <- liftIO $ doesDirectoryExist dbFile
  isFile <- liftIO $ doesFileExist dbFile
  case (isDir, isFile) of
    (True, _)  -> throwError $ "Input file \"" ++ dbFile ++ "\" is a directory"
    (_, True)  -> openDatabase dbFile
    (_, False) -> openDatabaseAndInit dbFile

notesackShutdown :: ExceptM ()
notesackShutdown = closeDatabase



