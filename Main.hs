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
-- inputs / hot keys / wtvr related to input stuff
data SackConfig = SackConfig

data Env = Env {
  envDb         :: Database,
  envVty        :: Vty,
  envSackConfig :: SackConfig
}

data State = State View

askDb :: Sack Database
askDb = envDb <$> ask

askVty :: Sack Vty
askVty = envVty <$> ask

askSackConfig :: Sack SackConfig
askSackConfig = envSackConfig <$> ask

getView :: Sack View
getView = do (State ret) <- get
             return ret

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
  let initEnv = Env Database vty SackConfig
      initState = State (View 0 (0,0) 0)
  execRWST (sackInteract False) initEnv initState >> return ()  
  notesackShutdown

mainExcept _ _ = throwError "Usage: notesack FILE"

sackInteract :: Bool -> Sack ()
sackInteract shouldExit = do
  unless shouldExit $ handleNextEvent >>= sackInteract

handleNextEvent = askVty >>= liftIO . nextEvent >>= handleEvent
  where handleEvent (EvKey (KChar 'a') []) = do
          lift $ addNote "mytext DROP TABLES bobby..." "my tag" 3 3 
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

addNote :: String -> String -> Int -> Int -> ExceptM ()
addNote text tag szX szY = libCall "could not add note" $ withCString2 text tag addIt
  where addIt text tag = i_add_note text tag (fromIntegral szX) (fromIntegral szY)
        withCString2 :: String -> String -> (CString -> CString -> IO a) -> IO a
        withCString2 x y f = do
          xx <- newCString x
          yy <- newCString y
          ret <- f xx yy
          free xx
          free yy
          return ret

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
foreign import ccall "interface.h i_add_note"
  i_add_note :: CString -> CString -> CInt -> CInt -> IO ()
foreign import ccall "interface.h i_close"
  i_close :: IO ()
foreign import ccall "interface.h i_err"
  i_err :: IO CInt
