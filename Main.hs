{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

import Control.Monad.Except

type ExceptM = ExceptT String IO

main = do maybeErr <- runExceptT setup
          case maybeErr of
            Left s  -> putStrLn s
            Right _ -> return ()

setup :: ExceptM ()
setup = do open "Hello"
           close

open :: String -> ExceptM ()
open str = libCall "could not open specified database" $ withCString str i_open

close :: ExceptM ()
close = libCall "error in close of db and so file" i_close

libCall :: String -> IO a -> ExceptM a
libCall errStr doIt = do
  ret     <- liftIO doIt
  errCode <- liftIO i_err
  if errCode /= 0
     then throwError errStr
     else return ret
  
foreign import ccall "interface.h i_open"
  i_open :: CString -> IO ()
foreign import ccall "interface.h i_close"
  i_close :: IO ()
foreign import ccall "interface.h i_err"
  i_err :: IO CInt
