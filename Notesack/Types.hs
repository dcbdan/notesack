module Notesack.Types (
  ExceptM, Sack,
  SackConfig(..), Env(..), State(..), Mode(..), SelectAction(..),
  TableView(..), TableViewNote(..), TableNote(..),
  Dir(..),Pos,Id,Box(..),
  askVty, askSackConfig, getView, getViewId, getMode, putMode,
  getCursor, putCursor, putMoveCursor, moveLoc
) where

import Control.Monad.RWS
import Control.Monad.Except

import Graphics.Vty hiding ( Mode, setMode )

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
--                      - i to enter insert mode
--                      - : to go to status mode
--   InserMode          - Escape to enter edit mode
--                      - typing modifies text contents of the box
data Mode = 
    BaseMode 
  | SelectMode (Pos,Pos) SelectAction
  | StatusMode
  | EditMode
  | InsertMode

data SelectAction = SNewNote

-- The region covered by Box l r u d
-- is [l,r] x [u,d]
data Box = Box Pos Pos Pos Pos

data Dir = DirL | DirR | DirU | DirD

-- things related to drawing boxes
-- inputs / hot keys / wtvr related to input stuff
data SackConfig = SackConfig

data Env = Env {
  envVty        :: Vty,
  envSackConfig :: SackConfig
}

data State = State { 
  tableView :: TableView,
  mode :: Mode,
  cursor :: (Pos,Pos),
  windowSize :: (Int,Int),
  notesInView :: [(Int, Image)]
}

-- We define the tables

data TableView = TableView {
  tvViewId :: String,
  tvLoc :: (Pos,Pos),
  tvSelected :: Maybe Id 
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

getView :: Sack TableView
getView = tableView <$> get

getViewId :: Sack String
getViewId = tvViewId <$> getView

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

moveLoc :: Dir -> (Pos,Pos) -> (Pos,Pos)
moveLoc DirL (x,y) = (x-1,y)
moveLoc DirR (x,y) = (x+1,y)
moveLoc DirU (x,y) = (x,y-1)
moveLoc DirD (x,y) = (x,y+1)

