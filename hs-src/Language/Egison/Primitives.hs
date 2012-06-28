module Language.Egison.Primitives where
import Language.Egison.Numerical
import Language.Egison.Parser
import Language.Egison.Types
import qualified Control.Exception
import Control.Monad.Error
import Data.Char hiding (isSymbol)
import Data.Array
import Data.Unique
import qualified Data.Map
import System.IO
import System.Directory (doesFileExist, removeFile)
import System.IO.Error

---------------------------------------------------
-- I/O Primitives
-- These primitives all execute within the IO monad
---------------------------------------------------
makePort :: IOMode -> [EgisonVal] -> IOThrowsError EgisonVal
makePort mode [(World actions), (String filename)] = do
  port <- liftM (Port filename) $ liftIO $ openFile filename mode
  let newWorld = case mode of
                   ReadMode -> World $ (OpenInputPort filename):actions
                   WriteMode -> World $ (OpenOutputPort filename):actions
  return $ makeTupleFromValList [newWorld, port]
makePort _ [] = throwError $ NumArgs 2 []
makePort _ [arg] = throwError $ NumArgs 2 [arg]
makePort _ args@(_:_:_:_) = throwError $ NumArgs 2 args
makePort _ _ = throwError $ Default $ "closePort: invalid arguments"

closePort :: [EgisonVal] -> IOThrowsError EgisonVal
closePort [World actions, Port filename port] = do
  liftIO $ hClose port
  let newWorld = World $ (ClosePort filename):actions
  return newWorld
closePort _ = throwError $ Default $ "closePort: invalid arguments"

writeChar :: [EgisonVal] -> IOThrowsError EgisonVal
writeChar [World actions, Port filename port, Char c] = do
  liftIO $ hPutChar port c
  let newWorld = World $ (WriteToPort filename [c]):actions
  return newWorld
writeChar _ = throwError $ Default $ "writeChar: invalid arguments"

writeString :: [EgisonVal] -> IOThrowsError EgisonVal
writeString [World actions, Port filename port, String str] = do
  liftIO $ hPutStr port str
  let newWorld = World $ (WriteToPort filename str):actions
  return newWorld
writeString _ = throwError $ Default $ "writeString: invalid arguments"

writeStringLine :: [EgisonVal] -> IOThrowsError EgisonVal
writeStringLine [World actions, Port filename port, String str] = do
  liftIO $ hPutStrLn port str
  let newWorld = World $ (WriteToPort filename str):actions
  return newWorld
writeStringLine _ = throwError $ Default $ "writeString: invalid arguments"

write :: [EgisonVal] -> IOThrowsError EgisonVal
write [World actions, Port filename port, val] = do
  let str = show val
  liftIO $ hPutStr port str
  let newWorld = World $ (WriteToPort filename str):actions
  return newWorld
write _ = throwError $ Default $ "write: invalid arguments"


readChar :: [EgisonVal] -> IOThrowsError EgisonVal
readChar [World actions, Port filename port] = do
  c <- liftIO $ hGetChar port
  let newWorld = World $ (ReadFromPort filename [c]):actions
  return $ makeTupleFromValList [newWorld, Char c]
readChar _ = throwError $ Default $ "getChar: invalid arguments"

readLine :: [EgisonVal] -> IOThrowsError EgisonVal
readLine [World actions, Port filename port] = do
  str <- liftIO $ hGetLine port
  let newWorld = World $ (ReadFromPort filename str):actions
  return $ makeTupleFromValList [newWorld, String str]
readLine _ = throwError $ Default $ "getLine: invalid arguments"

