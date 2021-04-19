{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Notesack.Database (
  openDatabaseAndInit, openDatabase, closeDatabase,
  hasView, addTv, addTvn, addTn,
  areaHasNote, maxNoteId, getNotesInArea,
  getNeighbors
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

addTv :: TableView -> ExceptM ()
addTv (TableView id (locx,locy) Nothing) = 
  libCall "could not add view" $ 
    withCString id $ \i -> add_view_no_selected i (fromIntegral locx) (fromIntegral locy)
addTv (TableView id (locx,locy) (Just selected)) = 
  libCall "could not add view" $ 
    withCString id $ \i -> add_view i (fromIntegral locx) (fromIntegral locy) (fromIntegral selected)


addTvn :: TableViewNote -> ExceptM ()
addTvn (TableViewNote viewId noteId' (Box l' r' u' d')) =
  let noteId = fromIntegral noteId'
      [l,r,u,d] = map fromIntegral [l',r',u',d']
   in libCall "could not add view, note pair" $
        withCString viewId $ \id ->
          add_view_note id noteId l r u d

withCStrings :: [String] -> ([CString] -> IO a) -> IO a
withCStrings xs f = recurse (reverse xs) []
  where recurse [] soFar = f soFar
        recurse (x:xs) soFar = withCString x (\ newX -> recurse xs (newX:soFar))

addTn :: TableNote -> ExceptM ()
addTn (TableNote noteId' text' created' changed') =
  libCall "could note add note" $
    withCStrings [text', created', changed'] $ \[text, created, changed] ->
      add_note (fromIntegral noteId') text created changed

openDatabaseAndInit :: String -> ExceptM ()
openDatabaseAndInit str = 
  do let err = "could not open and initialize database"
     libCall err $ withCString str i_open
     libCall err i_init

openDatabase :: String -> ExceptM ()
openDatabase str = libCall "could not open specified database" $ withCString str i_open

closeDatabase :: ExceptM ()
closeDatabase = libCall "error in close of db file" i_close

areaHasNote :: String -> Box -> ExceptM Bool
areaHasNote viewId (Box l r u d) =
  let (ll,rr,uu,dd) = (f l, f r, f u, f d)
      f = fromIntegral
   in (== 1) <$> (libCall "areaHasNote" $ withCString viewId $ area_has_note ll rr uu dd)

maxNoteId :: ExceptM Id
maxNoteId = fromIntegral <$> (libCall "maxNoteId" max_note_id)

-- TODO: is this SQL injection vulnerable? (yes)
getNotesInArea :: String -> Box -> ExceptM [(Box, String)]
getNotesInArea viewId (Box l r u d) = 
  let [sl,sr,su,sd] = map show [l,r,u,d]
      sql = unlines $ [
        "SELECT Text, LocL, LocR, LocU, LocD FROM Note JOIN ViewNote ",
        "WHERE MAX(LocL,"++sl++") <= MIN(LocR,"++sr++") ",
        "  AND MAX(LocU,"++su++") <= MIN(LocD,"++sd++") ",
        "  AND Note.NoteId == ViewNote.NoteId           ",
        "  AND ViewNote.ViewId == "++viewId++"         ;"]
      e = "getNotesInArea"
      fixRow (txt:positions) =
        let [l,r,u,d] = map fromObjInt positions
         in (Box l r u d, fromObjText txt)
   in map fixRow <$> getTable e sql [SqlText, SqlInt, SqlInt, SqlInt, SqlInt]

getNeighbors :: String -> Box -> ExceptM [Box]
getNeighbors viewId (Box l r u d) = 
  let [sl,sr,su,sd] = map show [l,r,u,d]
      sql = unlines $ [
        "SELECT LocL, LocR, LocU, LocD FROM ViewNote",
        "WHERE ViewNote.ViewId == "++viewId,
        "  AND MAX(LocL,"++sl++") <= MIN(LocR,"++sr++") ",
        "  AND MAX(LocU,"++su++") <= MIN(LocD,"++sd++") ",
        "  AND (LocL != "++sl++" OR LocR != "++sr++" OR ",
        "       LocU != "++su++" OR LocD != "++sd++");"]
      e = "getNeighbors"
      fixRow items = 
        let [a,b,c,d] = map fromObjInt items
         in Box a b c d
   in map fixRow <$> getTable e sql [SqlInt, SqlInt, SqlInt, SqlInt]

data SqlType = SqlInt | SqlText
data SqlObj = ObjInt Int | ObjText String
fromObjInt (ObjInt i) = i
fromObjText (ObjText s) = s
getTable :: String -> String -> [SqlType] -> ExceptM [[SqlObj]]
getTable err sql columns = do
  handle <- libCall err $ withCString sql stmt_init
  let incrementHandle = (==1) <$> (libCall err $ stmt_increment handle)
      getColumn (SqlInt, i) = (ObjInt . fromIntegral) <$> 
        (libCall err $ stmt_column_int handle i)
      getColumn (SqlText, i) = ObjText <$> 
        (libCall err (stmt_column_text handle i) >>= liftIO . peekCString)
      getInfo = mapM getColumn $ zip columns [0..]
  ret <- doUntil incrementHandle getInfo
  libCall err $ stmt_finalize handle
  return ret

doUntil :: Monad m => m Bool -> m a -> m [a]
doUntil checkIt doIt = 
  do stopNow <- checkIt
     if stopNow 
        then return []
        else do ret <- doIt
                rest <- doUntil checkIt doIt
                return (ret:rest)

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
foreign import ccall "interface.h add_view_note"
  add_view_note :: CString -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
foreign import ccall "interface.h add_note"
  add_note :: CInt -> CString -> CString -> CString -> IO ()
foreign import ccall "interface.h has_view"
  has_view :: CString -> IO CBool
foreign import ccall "interface.h area_has_note"
  area_has_note :: CInt -> CInt -> CInt -> CInt -> CString -> IO CBool
foreign import ccall "interface.h max_note_id"
  max_note_id :: IO CInt
----------------------------------------------------------------------------
foreign import ccall "interface.h i_exec"
  i_exec :: CString -> IO ()
foreign import ccall "interface.h stmt_init"
  stmt_init :: CString -> IO (Ptr ())
foreign import ccall "interface.h stmt_finalize"
  stmt_finalize :: Ptr () -> IO ()
foreign import ccall "interface.h stmt_increment"
  stmt_increment :: Ptr () -> IO CBool
foreign import ccall "interface.h stmt_column_int"
  stmt_column_int :: Ptr () -> CInt -> IO CInt
foreign import ccall "interface.h stmt_column_double"
  stmt_column_double :: Ptr () -> CInt -> IO CDouble
foreign import ccall "interface.h stmt_column_blob"
  stmt_column_blob :: Ptr () -> CInt -> IO (Ptr ())
foreign import ccall "interface.h stmt_column_text"
  stmt_column_text :: Ptr () -> CInt -> IO CString

