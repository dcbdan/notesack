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
import Graphics.Vty.Attributes ( defAttr, withBackColor, withForeColor, withStyle, bold )
import Graphics.Vty.Attributes.Color 

import System.Environment(getArgs)
import System.Directory

import Notesack.Types
import Notesack.Database
import Notesack.Misc 
import Notesack.Boundary
import Notesack.EditStr ( EditStr )
import qualified Notesack.EditStr as E
import Data.Char ( toLower )

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
  let initEnv = Env vty SackConfig
  today <- liftIO getDate
  initState <- getInitState today
  execRWST (drawSack >> sackInteract False) initEnv initState >> return ()  
  closeDatabase 
mainExcept _ _ = throwError "Usage: notesack FILE"

getInitState :: String -> ExceptM State
getInitState viewId = do
  -- create the view if it doesn't already exist
  loadView viewId 
  -- we need the location of the table view
  -- (it's a bit silly because we may have just
  --  added this row to the View table, but oh well)
  (loc@(locL,locU),tvCursor@(tvL,tvU)) <- lookupView viewId
  initWindowSize <- liftIO getWindowSize
  initNotesInView <- getInitNotes viewId loc initWindowSize
  unplacedNotes <- getUnplacedNotes viewId
  let initMode = 
        if null unplacedNotes
           then BaseMode
           else PlaceMode unplacedNotes
      initState = State 
        viewId 
        loc
        initMode 
        tvCursor
        initWindowSize
        initNotesInView
        ""
  return initState

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

-- Just move the cursor
handleEventMode mode (EvKey (KChar c) []) | baseOrPlace mode && c `elem` "hjkl" = 
  putMoveCursor (dirFromChar c) >> return False

-- Just move the view
-- (I'd like to use the control modifier, but for some reason, vty
--  wasn't picking up the ctrl + hkjl combo correctly for all of em)
handleEventMode mode (EvKey (KChar c) []) | baseOrPlace mode && c `elem` "HJKL" = 
  let dir = dirFromChar (toLower c)
   in (moveLoc dir <$> getViewLoc) 
        >>= resetViewLoc 
        >>  unlessM cursorOnScreen (putMoveCursor dir)
        >>  return False

-- Enter status mode
handleEventMode BaseMode (EvKey (KChar ':') []) = 
  putMode (StatusMode ":") >> return False
handleEventMode (StatusMode command) (EvKey (KChar c) []) =
  putMode (StatusMode (command++[c])) >> return False
-- Exit status mode by running the command
handleEventMode (StatusMode command) (EvKey KEnter []) = 
  runCommand command
handleEventMode (StatusMode command) (EvKey KBS []) =
  putMode (StatusMode (init command)) >> return False
-- Exit status mode by discarding the command
handleEventMode (StatusMode _) (EvKey KEsc []) = 
  putMode BaseMode >> return False

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
handleEventMode mode (EvKey (KChar ' ') []) | baseOrPlace mode = do
  let sAction = case mode of
                  BaseMode -> SNewNote
                  PlaceMode notes -> SPlace notes
  cursor@(l,u) <- getCursor
  viewId <- getViewId
  canSelect <- not <$> lift (areaHasNote viewId (Box l l u u))
  if canSelect 
    then putMode (SelectMode cursor sAction)
    else return ()
  return False

-- just leave select mode by escape
handleEventMode (SelectMode _ SNewNote) (EvKey KEsc []) = 
  putMode BaseMode >> return False
handleEventMode (SelectMode _ (SPlace notes)) (EvKey KEsc []) = 
  putMode (PlaceMode notes) >> return False

--  if we're deselecting, make sure the box is big enough. if so,
--  do the action on it
handleEventMode (SelectMode (xx,yy) action) (EvKey (KChar ' ') []) = do
  (x,y) <- getCursor
  let box@(Box l r u d) = toBox (x,y) (xx,yy)
  if r-l < 2 || d-u < 2
     then return ()
     else case action of
            SNewNote         -> do addNewNoteToView box 
                                   putMode BaseMode
            SPlace [id]      -> do placeNoteToView id box
                                   putMode BaseMode
            SPlace (id:rest) -> do placeNoteToView id box
                                   putMode (PlaceMode rest)
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

shutdownSack :: Sack ()
shutdownSack = saveViewInfo

saveViewInfo :: Sack ()
saveViewInfo = do 
  viewId <- getViewId 
  viewLoc <- getViewLoc
  cursorLoc <- getCursor
  lift $ saveView viewId viewLoc cursorLoc

data Command = 
    CommandTag String
  | CommandUntag (Maybe String)
  | CommandClose
  | CommandQuit
  | CommandView String
  | CommandWhich 
  | CommandToday
  | CommandPrevDay
  | CommandNextDay
  | CommandArchive
  | NoCommand

parseCommand :: String -> Command
parseCommand (':':xs) = recurse $ words xs
  where recurse ("tag":tag:[])   = CommandTag tag
        recurse ("t":tag:[])     = CommandTag tag
        recurse ("untag":tag:[]) = CommandUntag (Just tag)
        recurse ("untag":[])     = CommandUntag Nothing
        recurse ("view":tag:[])  = CommandView tag
        recurse ("v":tag:[])     = CommandView tag
        recurse ("close":[])     = CommandClose
        recurse ("c":[])         = CommandClose
        recurse ("which":[])     = CommandWhich
        recurse ("today":[])     = CommandToday
        recurse ("viewl":[])     = CommandPrevDay
        recurse ("viewr":[])     = CommandNextDay
        recurse ("archive":[])   = CommandArchive
        recurse ("a":[])         = CommandArchive
        recurse ("quit":[])      = CommandQuit  
        recurse ("q":[])         = CommandQuit
        recurse _                = NoCommand

runCommand :: String -> Sack Bool
runCommand commandStr = do
  let command = parseCommand commandStr
  case command of
    CommandTag   tag -> tagSelected   tag >> putMode BaseMode
    CommandUntag tag -> untagSelected tag >> putMode BaseMode
    CommandView  tag -> switchView tag
    CommandClose     -> closeSelected
    CommandToday     -> liftIO getDate >>= switchView
    CommandWhich     -> getViewId >>= putStatusError >> putMode BaseMode
    CommandPrevDay   -> switchViewPrevDay
    CommandNextDay   -> switchViewNextDay
    CommandArchive   -> archiveSelected >> putMode BaseMode
    CommandQuit      -> shutdownSack
    NoCommand        -> setStatusError "invalid command" >> putMode BaseMode
  case command of
    CommandQuit -> return True
    _           -> return False

switchView :: String -> Sack ()
switchView newViewId = do
  saveViewInfo
  newState <- lift $ getInitState newViewId
  put newState

switchViewNextDay :: Sack ()
switchViewNextDay = do
  viewId <- getViewId
  if isDateLike viewId
     then do nextTag <- lift $ getNextDay viewId
             case nextTag of
               (Just tag) -> switchView tag
               Nothing    -> putStatusError "no next day" >> putMode BaseMode
     else putStatusError "The current tag is not a date" >> putMode BaseMode

switchViewPrevDay :: Sack ()
switchViewPrevDay = do
  viewId <- getViewId
  if isDateLike viewId
     then do prevTag <- lift $ getPrevDay viewId
             case prevTag of
               (Just tag) -> switchView tag
               Nothing    -> putStatusError "no prev day" >> putMode BaseMode
     else putStatusError "The current tag is not a date" >> putMode BaseMode

tagSelected :: String -> Sack ()
tagSelected tag = 
  let f Nothing = return ()
      f (Just noteId) = do
        viewId <- getViewId
        if viewId == tag
           then -- any noteId that is selected already has the viewId as a tag!          
                setStatusError $ "The tag "++viewId++" is already in view!"
           else lift $ 
                   do -- create the view if it doesn't already exist
                      loadView tag
                      -- add to the tvn, but with a null box
                      addTvn (TableViewNote tag noteId nullBox)
   in if isDateLike tag 
         then setStatusError "Invalid tag. Reason: the tag is date like."
         else getSelected >>= f

untagSelected :: Maybe String -> Sack ()
untagSelected maybeTagToRemove = 
  let f _ Nothing = return ()
      f tagToRemove (Just noteId) = do
        viewId <- getViewId
        -- remove the tag from the note
        lift $ tvnRemove tagToRemove noteId
        -- if we just untagged a note from the current view, reset the screen
        if tagToRemove == viewId
           then do saveViewInfo
                   lift (getInitState viewId) >>= put
           else return ()
   in do tagToRemove <- case maybeTagToRemove of
                          Nothing -> getViewId
                          Just t  -> return t
         -- by not removing any notes that are datalike, you can't accidently
         -- delete a note
         if isDateLike tagToRemove
            then setStatusError "Invalid tag. Reason: the tag is date like."
            else getSelected >>= f tagToRemove

closeSelected :: Sack ()
closeSelected = 
  let f viewId Nothing = return ()
      f viewId (Just noteId) =  
        let isStatusMode (StatusMode _) = True
            isStatusMode _ = False
            err = "closeSelected must only be called from status mode"
         in do mode <- getMode
               throwErrorIf (not (isStatusMode mode)) err 
               -- give noteId a null box in tvn
               lift $ updateTvn viewId noteId nullBox
               -- Option 1: remove the note and update its neighbors
               -- Option 2: just reinit
               -- Option 2.
               saveViewInfo
               lift (getInitState viewId) >>= put
   in do viewId <- getViewId
         getSelected >>= f viewId

archiveSelected :: Sack ()
archiveSelected = 
  let f :: String -> Maybe Id -> Sack ()
      f viewId Nothing = return ()
      f viewId (Just noteId) = 
         do -- 0) get the tags of this note
            -- TODO: remove any empty views
            -- 1) remove all of the relevant entries in TableViewNote
            lift $ tvnRemoveNote noteId
            -- 2) reinit
            saveViewInfo
            lift (getInitState viewId) >>= put
   in do viewId <- getViewId
         getSelected >>= f viewId

getSelectedTags :: Sack [String]
getSelectedTags = do
  s <- getSelected
  case s of 
    Nothing -> return []
    (Just id) -> lift $ listSelectedTags id
    

getSelected :: Sack (Maybe Id)
getSelected = do
  viewId <- getViewId
  (l,u) <- getCursor
  allBoxesHere <- lift $ getNotesInArea viewId (Box l l u u)
  return $ case allBoxesHere of
             []        -> Nothing
             [(x,_,_)] -> Just x
             _         -> Nothing

setStatusError :: String -> Sack ()
setStatusError serr = putStatusError serr

-- Here is the idea:
--   Check to see of the cursor in not actually on the screen.
--   If it isn't, slide the view such that the cursor is on the edge of the screen.
repositionCursorOnScreen :: Sack ()
repositionCursorOnScreen = do
  (l,u) <- getViewLoc
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
    resetViewLoc (newL,newU)

resetViewLoc :: (Pos,Pos) -> Sack ()
resetViewLoc (newL,newU) = do
  (wx,wy) <- windowSize <$> get
  viewId <- getViewId
  newNotesInView <- lift $ getInitNotes viewId (newL,newU) (wx,wy)
  stateIn <- get
  put $ stateIn{ viewLoc = (newL,newU),
                 notesInView = newNotesInView }

cursorOnScreen :: Sack Bool
cursorOnScreen = do
  (l,u) <- getViewLoc
  (x,y) <- getCursor
  (wx,wy) <- windowSize <$> get
  return $ not (x >= l+wx || x < l || y >= u+wy || y < u)

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
  (wx,wy) <- windowSize <$> get
  viewId <- getViewId
  loc@(locL,locU) <- getViewLoc
  mode <- getMode
  let cursorObj = AbsoluteCursor (x-locL) (y-locU)
  modeImage <-
    case mode of
      (SelectMode (xx,yy) _)     -> return $ imageBox blue $ 
                                      shiftBox loc $ (toBox (x,y) (xx,yy))
      (EditMode _ box editStr _) -> toHighlightImage box (E.toLines editStr)
      _                          -> return emptyImage
  serr <- getStatusError
  -- clear the status error now
  putStatusError ""
  let boldAttr = withStyle defAttr bold
      statusImageNoShift =
        case (serr, mode) of
          ("", (SelectMode _ _)  ) -> I.string boldAttr "-- Select --"
          ("", (EditMode _ _ _ _)) -> I.string boldAttr "-- Edit --"
          ("", BaseMode          ) -> I.string boldAttr "-- Base --"
          ("", (PlaceMode _)     ) -> I.string boldAttr "-- Place --"
          ("", (StatusMode str)  ) -> I.string defAttr str
          (serr, _               ) -> I.string defAttr serr
      statusImage = translate 0 (wy-1) statusImageNoShift
      tagImage = translate 0 (wy-2) $ I.string defAttr $ "Tag: " ++ viewId      
  noteImages <- (map snd . notesInView) <$> get
 
  allTags <- lift $ listTags 
  tagsOfSelected <- getSelectedTags
  let toRowTagAttr = defAttr `withForeColor` rgbColor 0 0 255 `withBackColor` rgbColor 235 235 235
      toRowTag t = if t `elem` tagsOfSelected
                     then I.string (toRowTagAttr `withStyle` bold) t
                     else I.string toRowTagAttr t
  let barImage = I.vertCat (map toRowTag allTags)

  let allImages = statusImage:tagImage:barImage:modeImage:noteImages
      picture = (picForLayers allImages){ 
        picCursor = cursorObj, 
        picBackground = Background ' ' defAttr }
  liftIO $ update vty picture 

-- TODO: getInitNotes should just get all notes!!!!!!
--       also notesInView is a misnomer, its just all the notes..
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
  loc <- getViewLoc
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

toHighlightImage :: Box -> [String] -> Sack Image
toHighlightImage box lines = 
  let width     = boxWidth box - 2
      height    = boxHeight box - 2
      selectAttr = defAttr `withBackColor` rgbColor 102 102 255-- 255 255 180-- brightWhite 
      wrongAttr  = defAttr `withBackColor` red         
      lImg = I.vertCat $ map (I.char selectAttr) (replicate height ' ')
      rImg = I.vertCat $ map toImgIt items
        where isTooLong xs = length xs > width
              toImgIt (True,c)  = I.char wrongAttr c
              toImgIt (False,c) = I.char selectAttr c
              linesMod = lines ++ repeat ""
              items = zip (map isTooLong linesMod) (replicate height ' ')

      img = (I.vertCat $ map (I.string defAttr) lines) |> 
                    I.resize (boxWidth box - 2) (boxHeight box - 2)      
      tImg = I.string selectAttr $ replicate (width+2) ' '
      bImg = let attr = if length lines > height
                          then wrongAttr
                          else selectAttr
             in I.string selectAttr $ replicate (width+2) ' '
      imgNoShift = I.vertCat [tImg, I.horizCat [lImg, img, rImg], bImg]
   in do loc <- getViewLoc
         let (Box il ir iu id) = shiftBox loc box
         return $ translate il iu imgNoShift


shiftBox :: (Pos,Pos) -> Box -> Box
shiftBox (locL,locU) (Box l r u d) = Box (l-locL) (r-locL) (u-locU) (d-locU)

wrtLoc :: Box -> Sack Box
wrtLoc (Box l r u d) = do
  (locL,locU) <- getViewLoc
  return $ Box (l-locL) (r-locL) (u-locU) (d-locU)

addNewNoteToView :: Box -> Sack ()
addNewNoteToView box = do
  today <- liftIO getDate
  viewId <- getViewId
  viewLoc <- getViewLoc
  noteId <- newNoteId
  let tvn = TableViewNote viewId noteId box
      tn  = TableNote noteId "" today today
  lift $ addTvn tvn
  lift $ addTn  tn
  
  state <- get
  img <- lift $ toImageText viewLoc viewId box ""
  put state { notesInView = (noteId, img):(notesInView state) }

  -- we always add notes to the today view
  if today /= viewId
     then lift $ addTvn $ TableViewNote today noteId nullBox
     else return ()


placeNoteToView :: Id -> Box -> Sack ()
placeNoteToView noteId box = do
  viewId <- getViewId
  lift $ updateTvn viewId noteId box
  loc <- getViewLoc
  wSize <- windowSize <$> get
  newNotesInView <- lift $ getInitNotes viewId loc wSize
  state <- get
  put $ state { notesInView = newNotesInView }

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
whenM bool v = do
  maybeSo <- bool
  when maybeSo v

-- If we have the view, let it be the view,
-- otherwise, add the view
loadView :: String -> ExceptM ()
loadView viewId = 
  unlessM (hasView viewId) (addTv (TableView viewId (0,0) (0,0)))

notesackSetup :: String -> ExceptM ()
notesackSetup dbFile = do 
  isDir  <- liftIO $ doesDirectoryExist dbFile
  isFile <- liftIO $ doesFileExist dbFile
  case (isDir, isFile) of
    (True, _)  -> throwError $ "Input file \"" ++ dbFile ++ "\" is a directory"
    (_, True)  -> openDatabase dbFile
    (_, False) -> openDatabaseAndInit dbFile

onBoundary :: Box -> (Pos, Pos) -> Bool
onBoundary (Box ll rr uu dd) (l, u) = ll == l || rr == l || uu == u || dd == u

boxWidth  :: Box -> Int
boxWidth  (Box l r _ _) = r - l + 1

boxHeight  :: Box -> Int
boxHeight (Box _ _ u d) = d - u + 1

isEnterOrI event = event   == (EvKey KEnter []) || event == (EvKey (KChar 'i') []) 
isEnterOrEsc event = event == (EvKey KEnter []) || event == (EvKey KEsc [])

baseOrPlace BaseMode = True
baseOrPlace (PlaceMode _) = True
baseOrPlace _ = False
