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
import qualified Graphics.Vty.Image as I
import Graphics.Vty.Attributes ( defAttr, withBackColor )
import Graphics.Vty.Attributes.Color ( blue, green, rgbColor )

import System.Environment(getArgs)
import System.Directory

import Notesack.Types
import Notesack.Database
import Notesack.Misc 
import Notesack.Boundary

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
  notesackSetup filename
  today <- liftIO getDate
  loadView today
  initWindowSize <- liftIO getWindowSize
  initNotesInView <- getInitNotes today (0,0) initWindowSize
  let initEnv = Env vty SackConfig
      initPicture = Picture {
        picCursor = AbsoluteCursor 0 0,
        picLayers = map snd initNotesInView, 
        picBackground = Background ' ' defAttr }
      initState = State 
        (TableView today (0,0) Nothing) 
        (ModeSelect Nothing) 
        (0,0) 
        initWindowSize
        initNotesInView
  execRWST (drawSack >> sackInteract False) initEnv initState >> return ()  
  notesackShutdown
mainExcept _ _ = throwError "Usage: notesack FILE"

getInitNotes viewId (x,y) (nx, ny) = do
  notesInfo <- getNotesInArea viewId (Box x (x+nx-1) y (y+ny-1))
  let fix (noteId, box, text) = do
        img <- toImage viewId box text
        return (noteId, img)
  mapM fix notesInfo

toImage :: String -> Box -> String -> ExceptM Image
toImage viewId box@(Box il ir iu id) text = do
        neighbors <- getNeighbors viewId box
        let (top, sides, bot) = boundary box neighbors
            (lSide, rSide) = unzip sides
            lImg = I.vertCat $ map (I.char defAttr) lSide
            rImg = I.vertCat $ map (I.char defAttr) rSide
            tImg = I.string defAttr top
            bImg = I.string defAttr bot
            numN = length neighbors
            textImg = charFill defAttr (head $ show numN) (ir-il-1) (id-iu-1)
            img = I.vertCat [tImg, I.horizCat [lImg, textImg, rImg], bImg] |> translate il iu
        return img 


sackInteract :: Bool -> Sack ()
sackInteract shouldExit =
  unless shouldExit $ do
    exitNext <- handleNextEvent
    drawSack
    sackInteract exitNext

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
            boxSize (Box l r u d) = (r-l+1)*(d-u+1)
         in boxSize boxFini > boxSize boxInit
  in do (xx,yy) <- getCursor
        let region = selectRegion (xx,yy)
        viewId <- getViewId
        canMove <- if isExpanding (xx,yy)
                      then not <$> lift (areaHasNote viewId region)
                      else return True
        if canMove
           then putMoveCursor dir
           else return ()
        
-- Not selecting anything, so just move the cursor
handleModeSelect (EvKey (KChar c) []) Nothing | c `elem` "hjkl" = 
  let dir = dirFromChar c
   in putMoveCursor dir

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
                Just (xx,yy) -> (imageBox blue (toBox (x,y) (xx,yy)), AbsoluteCursor x y)
  noteImages <- (map snd . notesInView) <$> get
  
  let allImages = modeImage:noteImages
      picture = (picForLayers allImages){ 
        picCursor = cursorObj, 
        picBackground = Background ' ' defAttr }
  liftIO $ update vty picture 

-- this funciton is assuming box is in view
addNoteToView :: Box -> Sack ()
addNoteToView box = do
  today <- liftIO getDate
  viewId <- getViewId
  noteId <- newNoteId
  let tvn = TableViewNote viewId noteId box
      tn  = TableNote noteId "" today today
  lift $ addTvn tvn
  lift $ addTn  tn
  
  state <- get
  img <- lift $ toImage viewId box ""
  put state { notesInView = (noteId, img):(notesInView state) }

newNoteId :: Sack Id
newNoteId = (+1) <$> lift maxNoteId

imageBox :: Color -> Box -> Image
imageBox color (Box l r u d) = charFill attr ' ' (r-l+1) (d-u+1) |> translate l u
  where attr = withBackColor defAttr color
  
toBox (x1,y1) (x2,y2) = Box (min x1 x2) (max x1 x2) (min y1 y2) (max y1 y2)

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



