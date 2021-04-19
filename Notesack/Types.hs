module Notesack.Types (
  ExceptM, Sack,
  SackConfig(..), Env(..), State(..), Mode(..),
  TableView(..), TableViewNote(..), TableNote(..),
  Dir(..),Pos,Id,Box(..),
  askVty, askSackConfig, getView, getViewId, getMode, putMode,
  getCursor, putCursor, moveCursor, moveLoc
) where

import Control.Monad.RWS
import Control.Monad.Except

import Graphics.Vty hiding ( Mode, setMode )

type ExceptM = ExceptT String IO
type Sack = RWST Env () State ExceptM

type Pos = Int
type Id  = Int
data Mode = ModeSelect (Maybe (Pos,Pos))
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
  windowSize :: (Int,Int)
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

moveCursor :: Dir -> Sack ()
moveCursor direction = 
   do state <- get
      put state{ cursor = moveLoc direction (cursor state) }

moveLoc :: Dir -> (Pos,Pos) -> (Pos,Pos)
moveLoc DirL (x,y) = (x-1,y)
moveLoc DirR (x,y) = (x+1,y)
moveLoc DirU (x,y) = (x,y-1)
moveLoc DirD (x,y) = (x,y+1)

