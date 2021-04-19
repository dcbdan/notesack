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
import Graphics.Vty.Attributes.Color ( blue, green )

import System.Environment(getArgs)
import System.Directory

import Notesack.Types
import Notesack.Database
import Notesack.Misc 

--I'd prefer to use vty to get the inital window size,
--but even though they have a way to do it, it doesn't
--look like it is exposed in the api
import Notesack.WindowSize( getWindowSize )

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
  initWindowSize <- liftIO getWindowSize
  notesackSetup filename
  loadView today
  let initEnv = Env vty SackConfig
      initState = State 
        (TableView today (0,0) Nothing) 
        (ModeSelect Nothing) 
        (0,0) 
        initWindowSize
  execRWST (sackInteract False) initEnv initState >> return ()  
  notesackShutdown

mainExcept _ _ = throwError "Usage: notesack FILE"

sackInteract :: Bool -> Sack ()
sackInteract shouldExit = do
  unless shouldExit $ handleNextEvent >>= sackInteract

-- TODO finish adding a note
--
-- Things
-- 1) Draw all the notes in the view
-- 2) save on deselect (if size of box is big enough)
-- 3) only select if not on a note

handleNextEvent = askVty >>= liftIO . nextEvent >>= handleEvent
  where handleEvent (EvKey KEsc []) = return True
        handleEvent (EvResize nx ny) = do
          state <- get
          put state{ windowSize = (nx,ny) }
          return False
        handleEvent event = do
          mode <- getMode
          case mode of
            ModeSelect selectRegion -> handleModeSelect event selectRegion
          drawSack
          return False

handleModeSelect :: Event -> Maybe (Pos,Pos) -> Sack ()

-- On space, select or deselect.
-- If selecting, make sure that the cursor is not on a note
handleModeSelect (EvKey (KChar ' ') []) Nothing = do
  cursor@(l,u) <- getCursor
  viewId <- getViewId
  canSelect <- not <$> lift (areaHasNote viewId (Box l l u u))
  if canSelect 
    then putMode (ModeSelect (Just cursor))
    else return ()

-- On deselect, if a box is big enough, 
-- save it
-- A box is "big enough" if it is atleast 3 wide and 3 tall.
handleModeSelect (EvKey (KChar ' ') []) (Just (xx,yy)) = do
  (x,y) <- getCursor
  let box@(Box l r u d) = toBox (x,y) (xx,yy)
  if r-l < 2 || d-u < 2
     then return ()
     else addNoteToView box
  putMode (ModeSelect Nothing)

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
      isExpanding cursor =
        let boxInit = toBox (x,y) cursor
            boxFini = toBox (x,y) (moveLoc dir cursor)
            boxSize (Box l r u d) = (r-l)*(d-u)
         in boxSize boxFini > boxSize boxInit
  in do (xx,yy) <- getCursor
        let region = selectRegion (xx,yy)
        viewId <- getViewId
        canMove <- if isExpanding (xx,yy)
                      then not <$> lift (areaHasNote viewId region)
                      else return True
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
  let (modeImage, cursorObj) = 
        case maybeSelected of
                Nothing -> (emptyImage, AbsoluteCursor x y)
                Just (xx,yy) -> (selectImage (toBox (x,y) (xx,yy)), AbsoluteCursor x y)
  noteImages <- drawNotes
  let allImages = modeImage:noteImages
      picture = (picForLayers allImages){ picCursor = cursorObj }
  liftIO $ update vty picture 

drawNotes :: Sack [Image]
drawNotes = do
  (l,u) <- tvLoc <$> getView 
  (nx,ny) <- windowSize <$> get
  viewId <- getViewId
  notes <- lift $ getNotesInArea viewId (Box l (l+nx-1) u (u+ny-1))
  return $ map toImage notes
  where toImage :: (Box, String) -> Image
        toImage (Box l r u d, text) =
          let attr = withBackColor defAttr green
           in charFill attr ' ' (r-l+1) (d-u+1) |> translate l u

addNoteToView :: Box -> Sack ()
addNoteToView box = do
  today <- liftIO getDate
  viewId <- getViewId
  noteId <- newNoteId
  let tvn = TableViewNote viewId noteId box
      tn  = TableNote noteId "" today today
  lift $ addTvn tvn
  lift $ addTn  tn

newNoteId :: Sack Id
newNoteId = (+1) <$> lift maxNoteId

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
  unlessM (hasView viewId) (addTv (TableView viewId (0,0) Nothing))

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



