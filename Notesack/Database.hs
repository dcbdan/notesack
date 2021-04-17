{-# LANGUAGE ForeignFunctionInterface #-}

module Notesack.Database (
  openDatabaseAndInit, openDatabase, closeDatabase,
  hasView, addView, addViewNote, addNote
) where

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc ( free )

import Control.Monad.RWS
import Control.Monad.Except

import Notesack.Types

hasView :: String -> Bool
hasView = undefined

addView :: TableView -> ExceptM ()
addView (TableView id (locx,locy) Nothing) = 
  libCall "could not add view" $ 
    withCString id $ \id -> add_view_no_selected id (fromIntegral locx) (fromIntegral locy)
addView (TableView id (locx,locy) (Just selected)) = 
  libCall "could not add view" $ 
    withCString id $ \id -> add_view id (fromIntegral locx) (fromIntegral locy) (fromIntegral selected)


addViewNote :: TableViewNote -> ExceptM ()
addViewNote = undefined

addNote :: TableNote -> ExceptM ()
addNote = undefined

openDatabaseAndInit :: String -> ExceptM ()
openDatabaseAndInit str = libCall "could not open and initialize database" $ 
  withCString str i_open_and_init

openDatabase :: String -> ExceptM ()
openDatabase str = libCall "could not open specified database" $ withCString str i_open

closeDatabase :: ExceptM ()
closeDatabase = libCall "error in close of db file" i_close

libCall :: String -> IO CInt -> ExceptM ()
libCall errStr doIt = do
  errCode <- liftIO doIt
  if errCode /= 0
     then throwError $ errStr ++ " (error code: "++show errCode++")"
     else return ()
  
foreign import ccall "interface.h i_open"
  i_open :: CString -> IO CInt
foreign import ccall "interface.h i_open_and_init"
  i_open_and_init :: CString -> IO CInt
foreign import ccall "interface.h i_close"
  i_close :: IO CInt
foreign import ccall "interface.h add_view"
  add_view_no_selected :: CString -> CInt -> CInt -> IO CInt
foreign import ccall "interface.h add_view"
  add_view :: CString -> CInt -> CInt -> CInt -> IO CInt
