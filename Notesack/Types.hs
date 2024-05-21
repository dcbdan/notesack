module Notesack.Types (
  ExceptM, Sack,
  SackConfig(..), Env(..), State(..), Mode(..), SelectAction(..), EditState(..),
  TableView(..), TableViewNote(..), TableNote(..),
  Dir(..), Corner(..), Pos, Id, Box(..),
  askVty, askSackConfig, getViewId, getViewLoc, getMode, putMode,
  getStatusError, putStatusError,
  getCursor, putCursor, putMoveCursor, moveLoc, nullBox,
  throwErrorIf
) where

import Control.Monad.RWS
import Control.Monad.Except

import Graphics.Vty hiding ( Mode, setMode )
import Notesack.EditStr ( EditStr )

type ExceptM = ExceptT String IO
type Sack = RWST Env () State ExceptM

type Pos = Int

type Id  = Int

-- What are all the modes?
--   BaseMode           - move the cursor around, enter other modes
--                      - : to go to status mode
--                      - Enter to enter EditMode
--                      - Space to enter select mode
--   SelectMode         - select a region to do something
--                      - press escape to go to base mode
--                      - press space to do selection
--   StatusMode         - write stuff in the status bar
--                      - press escape twice to go to base
--   EditMode           - a box is selected, hjkl to navigate cursor on box
--                      - i to enter edit insert mode
--                      - : to go to status mode
--                      - v to enter edit visual mode
--                      - p to paste
--                      - Esc to go to base mode
--   EditMode.Insert    - Escape to enter edit mode
--                      - typing modifies text contents of the box
--   EditMode.Visual    - select a region and copy it
--   TODO: update this comment
data Mode =
    BaseMode
  | SelectMode (Pos,Pos) SelectAction
  | StatusMode String
  | EditMode Id Box EditStr EditState
  | PlaceMode [Id]

data SelectAction = SNewNote | SPlace [Id]
data EditState = EditInsert | EditVisual

-- The region covered by Box l r u d
-- is [l,r] x [u,d]
data Box = Box Pos Pos Pos Pos
nullBox = Box 0 0 0 0

data Dir = DirL | DirR | DirU | DirD

data Corner = CornerTL | CornerTR | CornerBL | CornerBR

-- things related to drawing boxes
-- inputs / hot keys / wtvr related to input stuff
data SackConfig = SackConfig

data Env = Env {
  envVty        :: Vty,
  envSackConfig :: SackConfig
}

-- the loc on the tableView is the global position
-- all boxes need to be global as well
-- all images need to be translated with respect to the tvLoc
-- the cursor is the global cursor
data State = State {
  viewId :: String,
  viewLoc :: (Pos,Pos),
  mode :: Mode,
  cursor :: (Pos,Pos),
  windowSize :: (Int,Int),
  notesInView :: [(Id, Image)],
  statusError :: String
}

-- We define the tables

data TableView = TableView {
  tvViewId :: String,
  tvLoc :: (Pos,Pos),
  tvCursor :: (Pos,Pos)
}

data TableViewNote = TableViewNote {
  tvnViewId :: String,
  tvnNoteId :: Id,
  tvnBox :: Box
}

data TableNote = TableNote {
  nNoteId :: Id,
  nText :: String,
  nDateCreated :: String,
  nDateChanged :: String
}

askVty :: Sack Vty
askVty = envVty <$> ask

askSackConfig :: Sack SackConfig
askSackConfig = envSackConfig <$> ask

getViewId :: Sack String
getViewId = viewId <$> get

getViewLoc :: Sack (Pos,Pos)
getViewLoc = viewLoc <$> get

getMode :: Sack Mode
getMode = mode <$> get

putMode :: Mode -> Sack ()
putMode m = do
  state <- get
  put state{ mode = m }

getCursor :: Sack (Pos, Pos)
getCursor = cursor <$> get

putCursor :: (Pos, Pos) -> Sack ()
putCursor c = do
  state <- get
  put state{ cursor = c }

putMoveCursor :: Dir -> Sack ()
putMoveCursor direction = do
  state <- get
  put state{ cursor = moveLoc direction (cursor state) }

putStatusError :: String -> Sack ()
putStatusError serr = do
  state <- get
  put state{ statusError = serr }

getStatusError :: Sack String
getStatusError = statusError <$> get

moveLoc :: Dir -> (Pos,Pos) -> (Pos,Pos)
moveLoc DirL (x,y) = (x-1,y)
moveLoc DirR (x,y) = (x+1,y)
moveLoc DirU (x,y) = (x,y-1)
moveLoc DirD (x,y) = (x,y+1)

throwErrorIf :: Bool -> String -> Sack ()
throwErrorIf True err = throwError err
throwErrorIf False _  = return ()
