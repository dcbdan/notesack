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

import Notesack.Types
import Notesack.Database
import Notesack.Misc 

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
  loadView today
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

unlessM bool v = do
  maybeSo <- bool
  unless maybeSo v

-- If we have the view, let it be the view,
-- otherwise, add the view
loadView :: String -> ExceptM ()
loadView viewId = 
  unlessM (hasView viewId) (addView (TableView viewId (0,0) Nothing))

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

