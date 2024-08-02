{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Notesack.Database (
  openDatabaseAndInit, openDatabase, closeDatabase,
  hasView, lookupView, addTv, addTvn, addTn, saveView,
  areaHasNote, maxNoteId, canPlace, getNotesInArea, getUnplacedNotes,
  getNeighbors, getFarIntervals, updateNote, updateTvn,
  getNextDay, getPrevDay, tvnRemove, tvnRemoveNote,
  getResizeNote,
  listTags, listSelectedTags
) where

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc ( free )

import Control.Monad.RWS
import Control.Monad.Except

import Notesack.Types
import Notesack.Misc


hasView :: String -> ExceptM Bool
hasView id = (==1) <$> (libCall "hasView" (withCString id has_view))

lookupView :: String -> ExceptM ((Pos,Pos), (Pos,Pos))
lookupView id =
  let sqlStr = "SELECT LocX, LocY, CurX, CurY FROM View WHERE ViewId == \"%w\";"
      sql = SqlQuery sqlStr [id]
      e = "lookupView"
      fixRow items =
        let [a,b,c,d] = map fromObjInt items
         in ((a,b),(c,d))
   in (fixRow . head) <$> getTable e sql [SqlInt, SqlInt, SqlInt, SqlInt]

saveView :: String -> (Pos,Pos) -> (Pos,Pos) -> ExceptM ()
saveView viewId (a,b) (c,d) =
  let sqlStr = unlines [
        "UPDATE View ",
        "SET LocX = "++show a ++ ",",
        "    LocY = "++show b ++ ",",
        "    CurX = "++show c ++ ",",
        "    CurY = "++show d,
        "WHERE ViewId = \"%w\";"]
   in exec "saveView" (SqlQuery sqlStr [viewId])

addTv :: TableView -> ExceptM ()
addTv (TableView id (a,b) (c,d)) =
  let sqlStr = unlines $ [
        "INSERT INTO View ( ViewId, LocX, LocY, CurX, CurY ) ",
        "VALUES( \"%w\","++show a++","++show b++","++show c++","++show d++");"]
      sql = SqlQuery sqlStr [id]
   in exec "add view" sql

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

getUnplacedNotes :: String -> ExceptM [Id]
getUnplacedNotes viewId =
  let sqlStr = unlines $ [
        "SELECT NoteId FROM ViewNote",
        " WHERE ViewId = \"%w\"",
        "   AND LocL = 0 AND LocR = 0 AND LocU = 0 AND LocD = 0;"]
      sql = SqlQuery sqlStr [viewId]
      e = "getUnplacedNotes"
      fixRow [x] = fromObjInt x
   in map fixRow <$> getTable e sql [SqlInt]

updateTvn :: String -> Int -> Box -> ExceptM ()
updateTvn viewId noteId (Box l r u d) =
  let [sl,sr,su,sd] = map show [l,r,u,d]
      sqlStr = unlines $ [
        "UPDATE ViewNote ",
        "SET LocL = "++show l ++ ",",
        "    LocR = "++show r ++ ",",
        "    LocU = "++show u ++ ",",
        "    LocD = "++show d,
        "WHERE ViewId = \"%w\"",
        "  AND NoteId = "++show noteId++";"]
   in exec "update tvn" (SqlQuery sqlStr [viewId])

canPlace :: String -> Id -> Box -> ExceptM Bool
canPlace viewId noteId (Box l r u d) =
  let [sl,sr,su,sd] = map show [l+1,r,u+1,d]
      sqlStr = unlines $ [
        "SELECT NoteId FROM ViewNote",
        "WHERE MAX(LocL+1,"++sl++") <= MIN(LocR,"++sr++") ", -- has intersection
        "  AND MAX(LocU+1,"++su++") <= MIN(LocD,"++sd++") ",
        "  AND (LocL != 0 OR LocR != 0 OR               ", -- remove null boxes
        "       LocU != 0 OR LocD != 0)                 ",
        "  AND NoteId != "++show noteId++"              ", -- is the right note
        "  AND ViewNote.ViewId == \"%w\";"]                -- is the right view
      sql = SqlQuery sqlStr [viewId]
      e = "canPlace"
      f [] = True
      f _  = False
   in f <$> getTable e sql [SqlInt]

getNotesInArea :: String -> Box -> ExceptM [(Id, Box, String)]
getNotesInArea viewId (Box l r u d) =
  let [sl,sr,su,sd] = map show [l,r,u,d]
      sqlStr = unlines $ [
        "SELECT Note.NoteId, Text, LocL, LocR, LocU, LocD FROM Note JOIN ViewNote ",
        "WHERE MAX(LocL,"++sl++") <= MIN(LocR,"++sr++") ", -- has intersection
        "  AND MAX(LocU,"++su++") <= MIN(LocD,"++sd++") ",
        "  AND (LocL != 0 OR LocR != 0 OR               ", -- remove null boxes
        "       LocU != 0 OR LocD != 0)                 ",
        "  AND Note.NoteId == ViewNote.NoteId           ", -- is the right note
        "  AND ViewNote.ViewId == \"%w\";"]                -- is the right view
      sql = SqlQuery sqlStr [viewId]
      e = "getNotesInArea"
      fixRow (which:txt:positions) =
        let [l,r,u,d] = map fromObjInt positions
         in (fromObjInt which,Box l r u d, fromObjText txt)
   in map fixRow <$> getTable e sql [SqlInt, SqlText, SqlInt, SqlInt, SqlInt, SqlInt]

getFarIntervals :: Dir -> String -> Pos -> ExceptM [(Pos,Pos)]
getFarIntervals dir viewId minV =
  let (select, condition) =
        case dir of
          DirL -> ("LocU, LocD", "LocL <= "++show minV)
          DirR -> ("LocU, LocD", "LocR >= "++show minV)
          DirU -> ("LocL, LocR", "LocU <= "++show minV)
          DirD -> ("LocL, LocR", "LocD >= "++show minV)
      sqlStr = unlines $ [
        "SELECT "++select++" FROM ViewNote",
        "WHERE ViewNote.ViewId == \"%w\"  ", -- is the right view
        "  AND "++condition++"            ", -- has a part past the boundary
        "  AND (LocL != 0 OR LocR != 0 OR ", -- remove null boxes
        "       LocU != 0 OR LocD != 0);  "]
      sql = SqlQuery sqlStr [viewId]
      e = "getFarIntervals"
      fixRow items =
        let [a,b] = map fromObjInt items
         in (a,b)
   in map fixRow <$> getTable e sql [SqlInt, SqlInt]

getNeighbors :: String -> Box -> ExceptM [Box]
getNeighbors viewId (Box l r u d) =
  let [sl,sr,su,sd] = map show [l,r,u,d]
      sqlStr = unlines $ [
        "SELECT LocL, LocR, LocU, LocD FROM ViewNote",
        "WHERE ViewNote.ViewId == \"%w\"",                 -- is the right view
        "  AND MAX(LocL,"++sl++") <= MIN(LocR,"++sr++") ", -- has intersection
        "  AND MAX(LocU,"++su++") <= MIN(LocD,"++sd++") ",
        "  AND (LocL != 0 OR LocR != 0 OR               ", -- remove null boxes
        "       LocU != 0 OR LocD != 0)                 ",
        "  AND (LocL != "++sl++" OR LocR != "++sr++" OR ", -- is not self
        "       LocU != "++su++" OR LocD != "++sd++");"]
      sql = SqlQuery sqlStr [viewId]
      e = "getNeighbors"
      fixRow items =
        let [a,b,c,d] = map fromObjInt items
         in Box a b c d
   in map fixRow <$> getTable e sql [SqlInt, SqlInt, SqlInt, SqlInt]

updateNote :: Id -> String -> ExceptM ()
updateNote noteId text =
  do today <- lift $ getDate
     exec "updateNote" (sql today)
  where sql today = SqlQuery (query today) [text]
        query today = unlines [
          "UPDATE Note ",
          "SET Text = \"%w\",",
          "    DateChanged = "++show today,
          "WHERE NoteId = "++show noteId++";"]

getNextDay :: String -> ExceptM (Maybe String)
getNextDay date =
  let sqlStr = unlines $ [
        "SELECT ViewId FROM View",
        "WHERE ViewId > \"%w\"",
        "ORDER BY ViewId ASC"]
      fixRow [x] = fromObjText x
   in do table <- filter isDateLike . map fixRow
                    <$> getTable "getNextDay" (SqlQuery sqlStr [date]) [SqlText]
         case table of
           []    -> return Nothing
           (x:_) -> return $ Just x

getPrevDay :: String -> ExceptM (Maybe String)
getPrevDay date =
  let sqlStr = unlines $ [
        "SELECT ViewId FROM View",
        "WHERE ViewId < \"%w\"",
        "ORDER BY ViewId DESC"]
      fixRow [x] = fromObjText x
   in do table <- filter isDateLike . map fixRow
                    <$> getTable "getPrevDay" (SqlQuery sqlStr [date]) [SqlText]
         case table of
           []    -> return Nothing
           (x:_) -> return $ Just x

tvnRemove :: String -> Id -> ExceptM ()
tvnRemove viewId noteId =
  let sqlStr = unlines $ [
        "DELETE FROM ViewNote",
        "WHERE ViewId = \"%w\"",
        "  AND NoteId = "++show noteId++";"]
   in exec "tvnRemove" $ SqlQuery sqlStr [viewId]

tvnRemoveNote :: Id -> ExceptM ()
tvnRemoveNote noteId =
  let sqlStr = unlines $ [
        "DELETE FROM ViewNote",
        "WHERE NoteId = "++show noteId++";"]
   in exec "tvnRemoveNote" $ SqlQuery sqlStr []

getResizeNote :: String -> (Pos, Pos) -> ExceptM (Maybe (Int, (Pos, Pos)))
getResizeNote view (cursorX, cursorY) =
  let sqlStrTopLeftNotes = unlines $ [
        "SELECT NoteId, LocL, LocU FROM ViewNote ",
        "WHERE ViewId = \"%w\"",
        "AND "++show cursorX++" > LocL AND "++show cursorY++" > LocU;"]
      sqlTopLeftNotes = SqlQuery sqlStrTopLeftNotes [view]
      sqlSchema3Ints = [SqlInt, SqlInt, SqlInt]
      fix3Ints items =
        let [a,b,c] = map fromObjInt items
         in (a,b,c)

      findCanPlace [] = throwError "could not place anything..."
      findCanPlace ((id,l,u):xs) = do
        success <- canPlace view id (Box l cursorX u cursorY)
        if success
          then return $ Just (id, (l,u))
          else findCanPlace xs

   in do notes <- map fix3Ints <$> getTable "TopLeftNotes" sqlTopLeftNotes sqlSchema3Ints
         case notes of
           [] -> return Nothing
           [(id,l,u)] -> return $ Just (id,(l,u))
           xs -> findCanPlace xs

-- Get all tags except the date tags
listTags :: ExceptM [String]
listTags =
  let sqlStr = unlines $ [
        "SELECT DISTINCT ViewNote.ViewId",
        "FROM ViewNote JOIN Note",
        "WHERE ViewNote.NoteId = Note.NoteId",
        "ORDER BY Note.DateChanged DESC;"]
      fixRow [x] = fromObjText x
   in filter (not . isDateLike) . map fixRow
        <$> getTable "listTags" (SqlQuery sqlStr []) [SqlText]

listSelectedTags :: Id -> ExceptM [String]
listSelectedTags id =
  let sqlStr = "SELECT DISTINCT ViewId FROM ViewNote WHERE NoteId = "++show id++";"
      fixRow [x] = fromObjText x
   in filter (not . isDateLike) . map fixRow
        <$> getTable "listSelectedTags" (SqlQuery sqlStr []) [SqlText]


data SqlQuery = SqlQuery String [String]
data SqlType = SqlInt | SqlText
data SqlObj = ObjInt Int | ObjText String
fromObjInt (ObjInt i) = i
fromObjText (ObjText s) = s
getTable :: String -> SqlQuery -> [SqlType] -> ExceptM [[SqlObj]]
getTable err sqlQ columns = withSqlQuery sqlQ $ \sql -> do
  handle <- libCall err $ stmt_init sql
  let incrementHandle = (==1) <$> (libCall err $ stmt_increment handle)
      getColumn (SqlInt, i) = (ObjInt . fromIntegral) <$>
        (libCall err $ stmt_column_int handle i)
      getColumn (SqlText, i) = ObjText <$>
        (libCall err (stmt_column_text handle i) >>= liftIO . peekCString)
      getInfo = mapM getColumn $ zip columns [0..]
  ret <- doUntil incrementHandle getInfo
  libCall err $ stmt_finalize handle
  return ret

-- TODO: does i_exec even do error thing?
exec :: String -> SqlQuery -> ExceptM ()
exec err sqlQ = withSqlQuery sqlQ $ \sql -> libCall err (i_exec sql)

withSqlQuery :: SqlQuery -> (CString -> ExceptM a) -> ExceptM a
withSqlQuery (SqlQuery sql []) f = do
  sqlC <- lift $ newCString sql
  ret <- f sqlC
  lift $ free sqlC
  return ret
withSqlQuery (SqlQuery sql [arg1]) f = do
  let newCString'   a   = lift $ newCString a
      mprintf1'     a b = lift $ mprintf1 a b
      free'         a   = lift $ free a
      free_mprintf' a   = lift $ free_mprintf a
  sqlC <- newCString' sql
  arg1 <- newCString' arg1
  sqlFinished <- mprintf1' sqlC arg1
  ret <- f sqlFinished
  free' sqlC
  free' arg1
  free_mprintf' sqlFinished
  return ret
withSqlQuery _ _ = error "Not implemented; Can only replace 1 string in a sql query"

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
foreign import ccall "interface.h mprintf1"
  mprintf1 :: CString -> CString -> IO CString
foreign import ccall "interface.h free_mprintf"
  free_mprintf :: CString -> IO ()
