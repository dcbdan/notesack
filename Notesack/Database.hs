{-# LANGUAGE ForeignFunctionInterface #-}

module Notesack.Database (
  openDatabaseAndInit, openDatabase, closeDatabase, libCall
) where

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc ( free )

import Control.Monad.RWS
import Control.Monad.Except

import Notesack.Types

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
