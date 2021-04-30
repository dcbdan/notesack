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
  -- create the view if it doesn't already exist
  loadView today
  -- we need the location of the table view
  -- (it's a bit silly because we may have just
  --  added this row to the View table, but oh well)
  loc <- lookupViewLoc today
  initWindowSize <- liftIO getWindowSize
  initNotesInView <- getInitNotes today loc initWindowSize
  let initEnv = Env vty SackConfig
      initPicture = Picture {
        picCursor = AbsoluteCursor 0 0,
        picLayers = map snd initNotesInView, 
        picBackground = Background ' ' defAttr }
      initState = State 
        (TableView today loc Nothing)
        BaseMode
        loc
        initWindowSize
        initNotesInView
  execRWST (drawSack >> sackInteract False) initEnv initState >> return ()  
  notesackShutdown
mainExcept _ _ = throwError "Usage: notesack FILE"

sackInteract :: Bool -> Sack ()
sackInteract shouldExit =
  unless shouldExit $ do
    exitNext <- handleNextEvent
    repositionCursorOnScreen
    drawSack
    sackInteract exitNext

handleNextEvent = askVty >>= liftIO . nextEvent >>= handleEvent
  where handleEvent event = getMode >>= (flip handleEventMode event)

handleEventMode :: Mode -> Event -> Sack Bool

-- This should use a status bar instead TODO
-- Save the location of the view and cursor, and then exit TODO
handleEventMode BaseMode (EvKey KEsc []) = return True

-- Just move the cursor
handleEventMode BaseMode (EvKey (KChar c) []) | c `elem` "hjkl" = 
  putMoveCursor (dirFromChar c) >> return False

-- Enter status mode
handleEventMode BaseMode (EvKey (KChar ':') []) = error "not implemented"

-- Try to enter edit mode
handleEventMode BaseMode event | isEnterOrI event = do
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
         let newMode = EditMode noteId box (E.fromText (boxWidth box - 2 ) text) EditInsert
         put state{ 
           notesInView = filter (fst .> (/= noteId)) (notesInView state),
           mode = newMode }
         handleInsertModeHelper newMode f >> return ()
           where f editStr cursor = 
                   let newCursor = E.snapCursor editStr cursor
                    in (newCursor, editStr)
  return False 

-- exit edit mode
handleEventMode (EditMode noteId box editStr EditInsert) (EvKey KEsc []) = do
  -- (1) put the note back into the cache
  -- (2) save it (TODO)
  img <- toImageLinesSack box (E.toLines editStr)
  state <- get
  put state{ notesInView = (noteId, img):(notesInView state),
             mode = BaseMode }
  lift $ updateNote noteId (E.toText editStr)
  return False

-- write c at the cursor location (in edit.insert)
handleEventMode 
  mode@(EditMode _ _ _ EditInsert)
  (EvKey (KChar c) []) = 
    handleInsertModeHelper mode f
      where f editStr cursor = E.insert editStr cursor c

-- Treat enter as a new line character in terms of edit.insert
handleEventMode 
  (EditMode a b c EditInsert) 
  (EvKey KEnter []) = 
    handleEventMode (EditMode a b c EditInsert) (EvKey (KChar '\n') [])

-- delete in edit.insert mode
handleEventMode
  mode@(EditMode _ _ _ EditInsert)
  (EvKey KDel []) =
    handleInsertModeHelper mode f
      where f = E.delete

-- backspace in edit.insert mode
handleEventMode
  mode@(EditMode _ _ _ EditInsert) 
  (EvKey KBS []) = 
    handleInsertModeHelper mode f
      where f = E.backspace

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

handleInsertModeHelper :: 
  Mode -> 
  (EditStr -> (Pos,Pos) -> ((Pos,Pos),EditStr)) -> 
  Sack Bool
handleInsertModeHelper
  (EditMode a box@(Box l r u d) editStr _)
  f = do
  (cl,cu) <- getCursor
  let (x,y) = (cl-(l+1), cu-(u+1))
      ((x',y'), newEditStr) = f editStr (x,y)
      (cl',cu') = (x'+(l+1), y'+(u+1))
  if cl' > l && cl' <= r && cu' > u && cu' < d
     then do putCursor (cl',cu')
             putMode (EditMode a box newEditStr EditInsert)
     else return ()
  return False

-- Here is the idea:
--   Check to see of the cursor in not actually on the screen.
--   If it isn't, slide the view such that the cursor is on the edge of the screen.
repositionCursorOnScreen :: Sack ()
repositionCursorOnScreen = do
  (TableView viewId (l,u) xx) <- getView
  (x,y) <- getCursor
  (wx,wy) <- windowSize <$> get
  when (x >= l+wx || x < l || y >= u+wy || y < u) $ do
    let newL = case (x < l, x >= l+wx) of
                 (True,_) -> x
                 (_,True) -> x + 1 - wx
                 _        -> l
        newU = case (y < u, y >= u+wy) of
                 (True,_) -> y
                 (_,True) -> y + 1 - wy
                 _        -> u
    newNotesInView <- lift $ getInitNotes viewId (newL,newU) (wx,wy)
    stateIn <- get
    put $ stateIn{ tableView = TableView viewId (newL,newU) xx,
                   notesInView = newNotesInView }

dirFromChar 'h' = DirL
dirFromChar 'j' = DirD
dirFromChar 'k' = DirU
dirFromChar 'l' = DirR

-- drawSack only operates with the State; it does not interact with
-- the database
drawSack :: Sack ()
drawSack = do
  vty <- askVty
  (x,y) <- getCursor
  loc@(locL,locU) <- tvLoc <$> getView
  mode <- getMode
  let cursorObj = AbsoluteCursor (x-locL) (y-locU)
  modeImage <-
    case mode of
      (SelectMode (xx,yy) _)     -> return $ imageBox blue $ 
                                      shiftBox loc $ (toBox (x,y) (xx,yy))
      (EditMode _ box editStr _) -> toImageLinesSack box (E.toLines editStr)
      _                          -> return emptyImage
  noteImages <- (map snd . notesInView) <$> get
  
  let allImages = modeImage:noteImages
      picture = (picForLayers allImages){ 
        picCursor = cursorObj, 
        picBackground = Background ' ' defAttr }
  liftIO $ update vty picture 

getInitNotes viewId (x,y) (nx, ny) = do
  notesInfo <- getNotesInArea viewId (Box x (x+nx-1) y (y+ny-1))
  let fix (noteId, box@(Box l r u d), text) = do
        img <- toImageText (x,y) viewId box text
        return (noteId, img)
  mapM fix notesInfo

-- Get the viewId, and the the location. Call toImageLines
toImageLinesSack :: Box -> [String] -> Sack Image
toImageLinesSack box lines = do
  viewId <- getViewId
  loc <- tvLoc <$> getView
  lift $ toImageLines loc viewId box lines

-- The ExceptM toImage functions are still with respect to the 
-- Sack coordinate system, hence passing in the loc argument.
-- This is so that getNeighbors can be called and make sense.
toImageText  :: (Pos,Pos) -> String -> Box -> String -> ExceptM Image
toImageText loc viewId box = 
  E.fromText (boxWidth box - 2) .> E.toLines .> toImageLines loc viewId box

toImageLines :: (Pos,Pos) -> String -> Box -> [String] -> ExceptM Image
toImageLines loc viewId box lines = 
  let img = (I.vertCat $ map (I.string defAttr) lines) |> 
              I.resize (boxWidth box - 2) (boxHeight box - 2)
   in toImage' loc viewId box img 

toImage' loc viewId box textImg = do
        let (Box il ir iu id) = shiftBox loc box
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

shiftBox :: (Pos,Pos) -> Box -> Box
shiftBox (locL,locU) (Box l r u d) = Box (l-locL) (r-locL) (u-locU) (d-locU)

wrtLoc :: Box -> Sack Box
wrtLoc (Box l r u d) = do
  (locL,locU) <- tvLoc <$> getView
  return $ Box (l-locL) (r-locL) (u-locU) (d-locU)

-- this funciton is assuming box is in view
addNoteToView :: SelectAction -> Box -> String -> Sack ()
addNoteToView _ box text = do
  today <- liftIO getDate
  viewId <- getViewId
  viewLoc <- tvLoc <$> getView
  noteId <- newNoteId
  let tvn = TableViewNote viewId noteId box
      tn  = TableNote noteId "" today today
  lift $ addTvn tvn
  lift $ addTn  tn
  
  state <- get
  img <- lift $ toImageText viewLoc viewId box text
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

isEnterOrI event = event == (EvKey KEnter []) || event == (EvKey (KChar 'i') []) 

