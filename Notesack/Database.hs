{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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


hasView :: String -> ExceptM Bool
hasView id = (==1) <$> (libCall "hasView" (withCString id has_view))

addView :: TableView -> ExceptM ()
addView (TableView id (locx,locy) Nothing) = 
  libCall "could not add view" $ 
    withCString id $ \i -> add_view_no_selected i (fromIntegral locx) (fromIntegral locy)
addView (TableView id (locx,locy) (Just selected)) = 
  libCall "could not add view" $ 
    withCString id $ \i -> add_view i (fromIntegral locx) (fromIntegral locy) (fromIntegral selected)


addViewNote :: TableViewNote -> ExceptM ()
addViewNote = undefined

addNote :: TableNote -> ExceptM ()
addNote = undefined

openDatabaseAndInit :: String -> ExceptM ()
openDatabaseAndInit str = 
  do let err = "could not open and initialize database"
     libCall err $ withCString str i_open
     libCall err i_init

openDatabase :: String -> ExceptM ()
openDatabase str = libCall "could not open specified database" $ withCString str i_open

closeDatabase :: ExceptM ()
closeDatabase = libCall "error in close of db file" i_close

libCall :: String -> IO a -> ExceptM a
libCall errStr doIt = do
  ret     <- liftIO doIt
  errCode <- liftIO i_error
  if errCode /= 0
     then throwError $ errStr ++ " (error code: "++show errCode++")"
     else return ret
  
foreign import ccall "interface.h i_open"
  i_open :: CString -> IO ()
foreign import ccall "interface.h i_init"
  i_init :: IO ()
foreign import ccall "interface.h i_close"
  i_close :: IO ()
foreign import ccall "interface.h i_error"
  i_error :: IO CInt
foreign import ccall "interface.h add_view_no_selected"
  add_view_no_selected :: CString -> CInt -> CInt -> IO ()
foreign import ccall "interface.h add_view"
  add_view :: CString -> CInt -> CInt -> CInt -> IO ()
foreign import ccall "interface.h has_view"
  has_view :: CString -> IO CBool
