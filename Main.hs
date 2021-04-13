{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

import Control.Monad.RWS
import Control.Monad.Except

import Graphics.Vty

import System.Environment(getArgs)
import System.Directory

type ExceptM = ExceptT String IO
type Sack = RWST Env () State ExceptM

type Fd = Ptr ()
data Box = Box Int Int Int Int
type Pos = Int
data Dir = DirL | DirR | DirU | DirD

data Database = Database
--  loadView :: String -> View,
--  saveView :: View -> IO (),
--  notesAt  :: View -> Box -> IO [Note],
--  select   :: View -> Dir -> IO View,
--  push     :: View -> Dir -> IO View

data View = View {
  viewId         :: Int,
  viewLoc        :: (Pos, Pos),
  viewSelectedId :: Int
}

data Note = Note {
  noteId       :: Int,
  noteLines    :: [String],
  noteLocation :: Box
}

-- things related to drawing boxes
data OutputConfig = OutputConfig

-- inputs / hot keys / wtvr related to input stuff
data InputConfig = InputConfig

data Env = Env {
  envDb        :: Database,
  envVty       :: Vty,
  envInConfig  :: InputConfig,
  envOutConfig ::  OutputConfig
}

data State = State View

askDb :: Sack Database
askDb = envDb <$> ask

askVty :: Sack Vty
askVty = envVty <$> ask

askInConfig :: Sack InputConfig
askInConfig = envInConfig <$> ask

askOutConfig :: Sack OutputConfig
askOutConfig = envOutConfig <$> ask

getView :: Sack View
getView = do (State ret) <- get
             return ret

main :: IO ()
main = do args <- getArgs
          maybeErr <- runExceptT (mainExcept args)
          case maybeErr of
            Left s  -> putStrLn s
            Right _ -> return ()

mainExcept [filename] = do
  vty <- notesackSetup filename
  let initEnv = Env Database vty InputConfig OutputConfig
      initState = State (View 0 (0,0) 0)
  execRWST (sackInteract False) initEnv initState >> return ()  
  notesackShutdown vty  
mainExcept _ = throwError "Usage: notesack FILE"


-- Next up: generate a view with a bunch of boxes...

-- The database contains:
-- View:
--   ViewId, LocX, LocY, whichSelected
-- ViewNote:
--   ViewId, NoteId, LocX, LocY, Tag
-- Notes:
--   NoteId, Text, DateCreated, DateChanged, Tags, SizeX, SizeY

sackInteract :: Bool -> Sack ()
sackInteract shouldExit = do
  unless shouldExit $ handleNextEvent >>= sackInteract

handleNextEvent = askVty >>= liftIO . nextEvent >>= handleEvent
  where handleEvent e = return $ e == EvKey KEsc []

--------------------------------------------------------------------------------------

notesackSetup :: String -> ExceptM (Vty)
notesackSetup dbFile = do 
  isDir  <- liftIO $ doesDirectoryExist dbFile
  isFile <- liftIO $ doesFileExist dbFile
  case (isDir, isFile) of
    (True, _)  -> throwError $ "Input file \"" ++ dbFile ++ "\" is a directory"
    (_, True)  -> openDatabase dbFile
    (_, False) -> openDatabaseAndInit dbFile
  liftIO $ mkVty defaultConfig

notesackShutdown :: Vty -> ExceptM ()
notesackShutdown vty = do
  closeDatabase
  liftIO $ shutdown vty

--------------------------------------------------------------------------------------

openDatabaseAndInit :: String -> ExceptM ()
openDatabaseAndInit str = libCall "could not open and initialize database" $ 
  withCString str i_open_and_init

openDatabase :: String -> ExceptM ()
openDatabase str = libCall "could not open specified database" $ withCString str i_open

closeDatabase :: ExceptM ()
closeDatabase = libCall "error in close of db and so file" i_close

libCall :: String -> IO a -> ExceptM a
libCall errStr doIt = do
  ret     <- liftIO doIt
  errCode <- liftIO i_err
  if errCode /= 0
     then throwError errStr
     else return ret
  
foreign import ccall "interface.h i_open"
  i_open :: CString -> IO ()
foreign import ccall "interface.h i_open_and_init"
  i_open_and_init :: CString -> IO ()
foreign import ccall "interface.h i_close"
  i_close :: IO ()
foreign import ccall "interface.h i_err"
  i_err :: IO CInt
