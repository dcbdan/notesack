{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc ( free )

import Control.Monad.RWS
import Control.Monad.Except

import Graphics.Vty

import System.Environment(getArgs)
import System.Directory

import Misc 

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
  let initEnv = Env vty SackConfig
      initState = State today
  execRWST (sackInteract False) initEnv initState >> return ()  
  notesackShutdown

mainExcept _ _ = throwError "Usage: notesack FILE"

sackInteract :: Bool -> Sack ()
sackInteract shouldExit = do
  unless shouldExit $ handleNextEvent >>= sackInteract

handleNextEvent = askVty >>= liftIO . nextEvent >>= handleEvent
  where handleEvent (EvKey (KChar 'a') []) = do
          return False
        handleEvent e = return $ e == EvKey KEsc []

--------------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------------

openDatabaseAndInit :: String -> ExceptM ()
openDatabaseAndInit str = libCall "could not open and initialize database" $ 
  withCString str i_open_and_init

openDatabase :: String -> ExceptM ()
openDatabase str = libCall "could not open specified database" $ withCString str i_open

closeDatabase :: ExceptM ()
closeDatabase = libCall "error in close of db file" i_close

libCall :: String -> IO a -> ExceptM a
libCall errStr doIt = do
  ret     <- liftIO doIt
  errCode <- liftIO i_err
  if errCode /= 0
     then throwError $ errStr ++ " (error code: "++show errCode++")"
     else return ret
  
foreign import ccall "interface.h i_open"
  i_open :: CString -> IO ()
foreign import ccall "interface.h i_open_and_init"
  i_open_and_init :: CString -> IO ()
foreign import ccall "interface.h i_close"
  i_close :: IO ()
foreign import ccall "interface.h i_err"
  i_err :: IO CInt

