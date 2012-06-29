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
writeChar [World actions, Char c] = do
  liftIO $ putChar c
  let newWorld = World $ (WriteToPort "stdout" [c]):actions
  return newWorld
writeChar _ = throwError $ Default $ "writeChar: invalid arguments"

writeString :: [EgisonVal] -> IOThrowsError EgisonVal
writeString [World actions, String str] = do
  liftIO $ putStr str
  let newWorld = World $ (WriteToPort "stdout" str):actions
  return newWorld
writeString _ = throwError $ Default $ "writeString: invalid arguments"

writeStringLine :: [EgisonVal] -> IOThrowsError EgisonVal
writeStringLine [World actions, String str] = do
  liftIO $ putStrLn str
  let newWorld = World $ (WriteToPort "stdout" str):actions
  return newWorld
writeStringLine _ = throwError $ Default $ "writeStringLine: invalid arguments"

write :: [EgisonVal] -> IOThrowsError EgisonVal
write [World actions, val] = do
  let str = show val
  liftIO $ putStr str
  let newWorld = World $ (WriteToPort "stdout" str):actions
  return newWorld
write _ = throwError $ Default $ "write: invalid arguments"

flushStdout :: [EgisonVal] -> IOThrowsError EgisonVal
flushStdout [World actions] = do
  liftIO $ hFlush stdout
  let newWorld = World $ (FlushPort "stdout"):actions
  return newWorld
flushStdout _ = throwError $ Default $ "flush: invalid arguments"

readChar :: [EgisonVal] -> IOThrowsError EgisonVal
readChar [World actions] = do
  c <- liftIO $ getChar
  let newWorld = World $ (ReadFromPort "stdin" [c]):actions
  return $ makeTupleFromValList [newWorld, Char c]
readChar _ = throwError $ Default $ "readChar: invalid arguments"

readLine :: [EgisonVal] -> IOThrowsError EgisonVal
readLine [World actions] = do
  str <- liftIO $ getLine
  let newWorld = World $ (ReadFromPort "stdin" str):actions
  return $ makeTupleFromValList [newWorld, String str]
readLine _ = throwError $ Default $ "readLine: invalid arguments"

readFromStdin :: [EgisonVal] -> IOThrowsError EgisonVal
readFromStdin [World actions] = do
  str <- liftIO $ hGetExpr stdin
  let newWorld = World $ (ReadFromPort "stdin" str):actions
  expr <- liftThrows $ readExpr str
  val <- liftThrows $ exprToVal expr
  return $ makeTupleFromValList [newWorld, val]
readFromStdin _ = throwError $ Default $ "read: invalid arguments"


writeCharToPort :: [EgisonVal] -> IOThrowsError EgisonVal
writeCharToPort [World actions, Port filename port, Char c] = do
  liftIO $ hPutChar port c
  let newWorld = World $ (WriteToPort filename [c]):actions
  return newWorld
writeCharToPort _ = throwError $ Default $ "writeCharToPort: invalid arguments"

writeStringToPort :: [EgisonVal] -> IOThrowsError EgisonVal
writeStringToPort [World actions, Port filename port, String str] = do
  liftIO $ hPutStr port str
  let newWorld = World $ (WriteToPort filename str):actions
  return newWorld
writeStringToPort _ = throwError $ Default $ "writeString: invalid arguments"

writeStringLineToPort :: [EgisonVal] -> IOThrowsError EgisonVal
writeStringLineToPort [World actions, Port filename port, String str] = do
  liftIO $ hPutStrLn port str
  let newWorld = World $ (WriteToPort filename str):actions
  return newWorld
writeStringLineToPort _ = throwError $ Default $ "writeStringLineToPort: invalid arguments"

writeToPort :: [EgisonVal] -> IOThrowsError EgisonVal
writeToPort [World actions, Port filename port, val] = do
  let str = show val
  liftIO $ hPutStr port str
  let newWorld = World $ (WriteToPort filename str):actions
  return newWorld
writeToPort _ = throwError $ Default $ "writeToPort: invalid arguments"

flushPort :: [EgisonVal] -> IOThrowsError EgisonVal
flushPort [World actions, Port filename port] = do
  liftIO $ hFlush port
  let newWorld = World $ (FlushPort filename):actions
  return newWorld
flushPort _ = throwError $ Default $ "flush-port: invalid arguments"

readCharFromPort :: [EgisonVal] -> IOThrowsError EgisonVal
readCharFromPort [World actions, Port filename port] = do
  c <- liftIO $ hGetChar port
  let newWorld = World $ (ReadFromPort filename [c]):actions
  return $ makeTupleFromValList [newWorld, Char c]
readCharFromPort _ = throwError $ Default $ "readCharFromPort: invalid arguments"

readLineFromPort :: [EgisonVal] -> IOThrowsError EgisonVal
readLineFromPort [World actions, Port filename port] = do
  str <- liftIO $ hGetLine port
  let newWorld = World $ (ReadFromPort filename str):actions
  return $ makeTupleFromValList [newWorld, String str]
readLineFromPort _ = throwError $ Default $ "readLineFromPort: invalid arguments"

readFromPort :: [EgisonVal] -> IOThrowsError EgisonVal
readFromPort [World actions, Port filename port] = do
  str <- liftIO $ hGetExpr port
  let newWorld = World $ (ReadFromPort filename str):actions
  expr <- liftThrows $ readExpr str
  val <- liftThrows $ exprToVal expr
  return $ makeTupleFromValList [newWorld, val]
readFromPort _ = throwError $ Default $ "read: invalid arguments"


hGetExpr :: Handle -> IO String
hGetExpr h = do
  str <- loop ""
  return str
 where
    loop :: String -> IO String
    loop input0 = do
      input <- hGetLine h
      let newInput = input0 ++ input
      if countParens newInput
        then return newInput
        else loop newInput

countParens :: String -> Bool
countParens str = let countOpen = length $ filter (\c -> ('(' == c)
                                                      || ('{' == c)
                                                      || ('[' == c)
                                                      || ('<' == c))
                                                  str in
                  let countClose = length $ filter (\c -> (')' == c)
                                                       || ('}' == c)
                                                       || (']' == c)
                                                       || ('>' == c))
                                                  str in
                    (countOpen == countClose)

exprToVal :: EgisonExpr -> ThrowsError EgisonVal
exprToVal (BoolExpr contents) = return $ Bool contents
exprToVal (CharExpr contents) = return $ Char contents
exprToVal (StringExpr contents) = return $ String contents
exprToVal (NumberExpr contents) = return $ Number contents
exprToVal (FloatExpr contents) = return $ Float contents
exprToVal (InductiveDataExpr cons argExprs) = do
  args <- mapM exprToVal argExprs
  return $ InductiveData cons args
exprToVal (CollectionExpr innerExprs) = do
  innerVals <- mapM innerExprToInnerVal innerExprs
  return $ Collection innerVals
exprToVal (TupleExpr innerExprs) = do
  innerVals <- mapM innerExprToInnerVal innerExprs
  return $ Tuple innerVals
exprToVal _ = throwError $ Default "read: invalid value"

innerExprToInnerVal :: InnerExpr -> ThrowsError InnerVal
innerExprToInnerVal (ElementExpr expr) = exprToVal expr >>= return . Element
innerExprToInnerVal (SubCollectionExpr expr) = exprToVal expr >>= return . SubCollection
