module Notesack.Types (
  ExceptM, Sack,
  SackConfig(..), Env(..), State(..),
  TableView(..), TableViewNote(..), TableNote(..),
  askVty, askSackConfig, getViewId
) where

import Control.Monad.RWS
import Control.Monad.Except

import Graphics.Vty

type ExceptM = ExceptT String IO
type Sack = RWST Env () State ExceptM

type Pos = Int
type Id  = Int
data Box = Box Pos Pos Pos Pos
data Dir = DirL | DirR | DirU | DirD

-- things related to drawing boxes
-- inputs / hot keys / wtvr related to input stuff
data SackConfig = SackConfig

data Env = Env {
  envVty        :: Vty,
  envSackConfig :: SackConfig
}

data State = State { currentView :: String }

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

getViewId :: Sack String
getViewId = currentView <$> get


