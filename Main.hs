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
import Notesack.EditStr ( EditStr )
import qualified Notesack.EditStr as E

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
        BaseMode
        (0,0) 
        initWindowSize
        initNotesInView
  execRWST (drawSack >> sackInteract False) initEnv initState >> return ()  
  notesackShutdown
mainExcept _ _ = throwError "Usage: notesack FILE"

getInitNotes viewId (x,y) (nx, ny) = do
  notesInfo <- getNotesInArea viewId (Box x (x+nx-1) y (y+ny-1))
  let fix (noteId, box, text) = do
        img <- toImageText viewId box text
        return (noteId, img)
  mapM fix notesInfo

toImageText  :: String -> Box -> String -> ExceptM Image
toImageText viewId box = 
  E.fromText (boxWidth box - 2) .> E.toLines .> toImageLines viewId box

toImageLines :: String -> Box -> [String] -> ExceptM Image
toImageLines viewId box lines = 
  let img = (I.vertCat $ map (I.string defAttr) lines) |> 
              I.resize (boxWidth box - 2) (boxHeight box - 2)
   in toImage' viewId box img 

toImage' viewId box@(Box il ir iu id) textImg = do
        neighbors <- getNeighbors viewId box
        let (top, sides, bot) = boundary box neighbors
            (lSide, rSide) = unzip sides
            lImg = I.vertCat $ map (I.char defAttr) lSide
            rImg = I.vertCat $ map (I.char defAttr) rSide
            tImg = I.string defAttr top
            bImg = I.string defAttr bot
            numN = length neighbors
            img = I.vertCat [tImg, I.horizCat [lImg, textImg, rImg], bImg] |> translate il iu
        return img 

sackInteract :: Bool -> Sack ()
sackInteract shouldExit =
  unless shouldExit $ do
    exitNext <- handleNextEvent
    drawSack
    sackInteract exitNext

handleNextEvent = askVty >>= liftIO . nextEvent >>= handleEvent
  where handleEvent event = getMode >>= (flip handleEventMode event)

handleEventMode :: Mode -> Event -> Sack Bool

-- For now but this really shouldn't do anything TODO
handleEventMode BaseMode (EvKey KEsc []) = return True

-- Just move the cursor
handleEventMode BaseMode (EvKey (KChar c) []) | c `elem` "hjkl" = 
  putMoveCursor (dirFromChar c) >> return False

-- Enter status mode
handleEventMode BaseMode (EvKey (KChar ':') []) = error "not implemented"

-- Try to enter edit mode
handleEventMode BaseMode (EvKey KEnter []) = do
  viewId <- getViewId
  cursor@(l,u) <- getCursor
  notesInfo <- lift $ getNotesInArea viewId (Box l l u u)
  case notesInfo of
    -- there is no note here
    [] -> return ()
    -- trying to enter a note on a boundary
    (_:_:_) -> return ()
    -- one note, but cursor on boundary
    [(_, box, _)] | onBoundary box cursor -> return ()
    -- one note, cursor in correct region
    [(noteId, box, text)] -> 
      do -- 1) remove the note from the cache
         -- 2) update the mode
         state <- get
         put state{ 
          notesInView = filter (fst .> (/= noteId)) (notesInView state),
          mode = EditMode noteId box (E.fromText (boxWidth box - 2 ) text) EditBase }
  return False 

-- move the cursor but only within the boundaries of the box
handleEventMode 
  (EditMode _ box _ EditBase) 
  (EvKey (KChar c) []) | c `elem` "hjkl" =
    do aNewCursor <- moveLoc (dirFromChar c) <$> getCursor
       if onBoundary box aNewCursor
          then return False
          else putCursor aNewCursor >> return False

-- exit edit mode
handleEventMode (EditMode noteId box editStr EditBase) (EvKey KEsc []) = do
  -- (1) put the note back into the cache
  -- (2) save it (TODO)
  viewId <- getViewId
  img <- lift $ toImageLines viewId box (E.toLines editStr)
  state <- get
  put state{ notesInView = (noteId, img):(notesInView state),
             mode = BaseMode }
  return False

-- enter edit.insert mode
handleEventMode 
  (EditMode noteId box@(Box l r u d) editStr EditBase) 
  (EvKey (KChar 'i') []) = do
    (cl,cu) <- getCursor
    let (x,y) = (cl-(l+1), cu-(u+1))
        (x',y') = E.snapCursor editStr (x,y)
    putCursor (x'+(l+1), y'+(u+1))
    putMode (EditMode noteId box editStr EditInsert)
    return False

-- write c at the cursor location (in edit.insert)
handleEventMode 
  (EditMode a box@(Box l r u d) editStr EditInsert) 
  (EvKey (KChar c) []) = do
    (cl,cu) <- getCursor
    let (x,y) = (cl-(l+1), cu-(u+1))
        ((x',y'), newEditStr) = E.insert editStr (x,y) c
    putCursor (x'+(l+1), y'+(u+1))
    putMode (EditMode a box newEditStr EditInsert)
    return False

-- Treat enter as a new line character in terms of edit.insert
handleEventMode 
  (EditMode a b c EditInsert) 
  (EvKey KEnter []) = 
    handleEventMode (EditMode a b c EditInsert) (EvKey (KChar '\n') [])

-- delete in edit.insert mode
handleEventMode
  (EditMode a box@(Box l r u d) editStr EditInsert)
  (EvKey KDel []) = do
    (cl,cu) <- getCursor
    let (x,y) = (cl-(l+1), cu-(u+1))
        ((x',y'), newEditStr) = E.delete editStr (x,y) 
    putCursor (x'+(l+1), y'+(u+1))
    putMode (EditMode a box newEditStr EditInsert)
    return False

-- backspace in edit.insert mode
handleEventMode
  (EditMode a box@(Box l r u d) editStr EditInsert)
  (EvKey KBS []) = do
    (cl,cu) <- getCursor
    let (x,y) = (cl-(l+1), cu-(u+1))
        ((x',y'), newEditStr) = E.backspace editStr (x,y) 
    putCursor (x'+(l+1), y'+(u+1))
    putMode (EditMode a box newEditStr EditInsert)
    return False
-- TODO: abstract these functions

-- exit edit.insert mode
handleEventMode (EditMode a b c EditInsert) (EvKey KEsc []) =
  putMode (EditMode a b c EditBase) >> return False

-- enter select mode
handleEventMode BaseMode (EvKey (KChar ' ') []) = do
  cursor@(l,u) <- getCursor
  viewId <- getViewId
  canSelect <- not <$> lift (areaHasNote viewId (Box l l u u))
  if canSelect 
    then putMode (SelectMode cursor SNewNote)
    else return ()
  return False

-- just leave select mode by escape
handleEventMode (SelectMode _ _) (EvKey KEsc []) = putMode BaseMode >> return False

--  if we're deselecting, make sure the box is big enough. if so,
--  do the action on it
handleEventMode (SelectMode (xx,yy) action) (EvKey (KChar ' ') []) = do
  (x,y) <- getCursor
  let box@(Box l r u d) = toBox (x,y) (xx,yy)
  if r-l < 2 || d-u < 2
     then return ()
     else addNoteToView action box ""
  putMode BaseMode
  return False

-- movement in select mode;
-- make sure that expanding in the direction is not going to bump into
-- another box
handleEventMode (SelectMode (x,y) action) (EvKey (KChar c) []) | c `elem` "hjkl" = 
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
        return False

handleEventMode _ _ = return False

dirFromChar 'h' = DirL
dirFromChar 'j' = DirD
dirFromChar 'k' = DirU
dirFromChar 'l' = DirR

drawSack :: Sack ()
drawSack = do
  vty <- askVty
  (x,y) <- getCursor
  mode <- getMode
  let cursorObj = AbsoluteCursor x y
  modeImage <-
    case mode of
      (SelectMode (xx,yy) _)   -> return $ imageBox blue (toBox (x,y) (xx,yy))
      (EditMode _ box editStr _) -> do viewId <- getViewId
                                       lift $ toImageLines viewId box (E.toLines editStr)
      _                        -> return emptyImage
  noteImages <- (map snd . notesInView) <$> get
  
  let allImages = modeImage:noteImages
      picture = (picForLayers allImages){ 
        picCursor = cursorObj, 
        picBackground = Background ' ' defAttr }
  liftIO $ update vty picture 

-- this funciton is assuming box is in view
addNoteToView :: SelectAction -> Box -> String -> Sack ()
addNoteToView _ box text = do
  today <- liftIO getDate
  viewId <- getViewId
  noteId <- newNoteId
  let tvn = TableViewNote viewId noteId box
      tn  = TableNote noteId "" today today
  lift $ addTvn tvn
  lift $ addTn  tn
  
  state <- get
  img <- lift $ toImageText viewId box text
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

onBoundary :: Box -> (Pos, Pos) -> Bool
onBoundary (Box ll rr uu dd) (l, u) = ll == l || rr == l || uu == u || dd == u

boxWidth  :: Box -> Int
boxWidth  (Box l r _ _) = r - l + 1

boxHeight  :: Box -> Int
boxHeight (Box _ _ u d) = d - u + 1
