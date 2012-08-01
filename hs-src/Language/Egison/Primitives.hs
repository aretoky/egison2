module Language.Egison.Primitives where
import Language.Egison.Parser
import Language.Egison.Types
import Control.Monad.Error
import Control.Exception (try)
import Data.Char hiding (isSymbol)
import Data.Array
import Data.Unique
import qualified Data.Map
import System.IO
import System.Directory (doesFileExist, removeFile)
import System.IO.Error hiding (try)

import Paths_egison

---------------------------------------------------
-- I/O Primitives
---------------------------------------------------
getLibDirName :: [EgisonVal] -> IOThrowsError EgisonVal
getLibDirName [] = liftIO $ (getDataFileName "") >>= return . String
getLibDirName _ = throwError $ Default $ "readCharFromPort: invalid arguments"

makePort :: IOMode -> [EgisonVal] -> IOThrowsError EgisonVal
makePort mode [(World actions), (String filename)] = do
  port <- liftM (Port filename) $ liftIO $ openFile filename mode
  let newWorld = case mode of
                   ReadMode -> World $ (OpenInputPort filename):actions
                   WriteMode -> World $ (OpenOutputPort filename):actions
  return $ Tuple [newWorld, port]
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
  liftIO $ hSetBuffering stdin NoBuffering
  input <- liftIO $ try (liftIO getChar)
  liftIO $ hSetBuffering stdin LineBuffering
  case input of
    Left e -> if isEOFError e
                then do
                  let newWorld = World $ (ReadFromPort "stdin" "EOF"):actions
                  return $ Tuple [newWorld, EOF]
                else throwError $ Default "I/O error read-char"
    Right inpChr -> do
      let newWorld = World $ (ReadFromPort "stdin" [inpChr]):actions
      return $ Tuple [newWorld, Char inpChr]
readChar _ = throwError $ Default $ "readChar: invalid arguments"

readLine :: [EgisonVal] -> IOThrowsError EgisonVal
readLine [World actions] = do
  input <- liftIO $ try (liftIO getLine)
  case input of
    Left e -> if isEOFError e
                then do
                  let newWorld = World $ (ReadFromPort "stdin" "EOF"):actions
                  return $ Tuple [newWorld, EOF]
                else throwError $ Default "I/O error read-line"
    Right inpStr -> do
      let newWorld = World $ (ReadFromPort "stdin" inpStr):actions
      return $ Tuple [newWorld, String inpStr]
readLine _ = throwError $ Default $ "readLine: invalid arguments"

readFromStdin :: [EgisonVal] -> IOThrowsError EgisonVal
readFromStdin [World actions] = do
  str <- hGetExpr stdin
  let newWorld = World $ (ReadFromPort "stdin" str):actions
  expr <- liftThrows $ readExpr str
  val <- liftThrows $ exprToVal expr
  return $ Tuple [newWorld, val]
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
  liftIO $ hSetBuffering port NoBuffering
  input <- liftIO $ try (liftIO $ hGetChar port)
  liftIO $ hSetBuffering port LineBuffering
  case input of
    Left e -> if isEOFError e
                then do
                  let newWorld = World $ (ReadFromPort filename "EOF"):actions
                  return $ Tuple [newWorld, EOF]
                else throwError $ Default "I/O error read-char-from-port"
    Right inpChr -> do
      let newWorld = World $ (ReadFromPort filename [inpChr]):actions
      return $ Tuple [newWorld, Char inpChr]
readCharFromPort _ = throwError $ Default $ "readCharFromPort: invalid arguments"

readLineFromPort :: [EgisonVal] -> IOThrowsError EgisonVal
readLineFromPort [World actions, Port filename port] = do
  input <- liftIO $ try (liftIO $ hGetLine port)
  case input of
    Left e -> if isEOFError e
                then do
                  let newWorld = World $ (ReadFromPort filename "EOF"):actions
                  return $ Tuple [newWorld, EOF]
                else throwError $ Default "I/O error read-line-from-port"
    Right inpStr -> do
      let newWorld = World $ (ReadFromPort filename inpStr):actions
      return $ Tuple [newWorld, String inpStr]
readLineFromPort _ = throwError $ Default $ "readLineFromPort: invalid arguments"

readFromPort :: [EgisonVal] -> IOThrowsError EgisonVal
readFromPort [World actions, Port filename port] = do
  str <- hGetExpr port
  let newWorld = World $ (ReadFromPort filename str):actions
  expr <- liftThrows $ readExpr str
  val <- liftThrows $ exprToVal expr
  return $ Tuple [newWorld, val]
readFromPort _ = throwError $ Default $ "read: invalid arguments"


hGetExpr :: Handle -> IOThrowsError String
hGetExpr h = do
  str <- loop ""
  return str
 where
    loop :: String -> IOThrowsError String
    loop input0 = do
      input <- liftIO $ try (liftIO $ hGetLine h)
      case input of
        Left e -> if isEOFError e
                    then throwError $ Default "EOF error read or read-from-port"
                    else throwError $ Default "I/O error read or read-from-port"
        Right inpStr ->
          let newInput = input0 ++ inpStr in
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
  vals <- innerExprsToVals innerExprs
  return $ Collection vals
exprToVal (TupleExpr innerExprs) = do
  vals <- innerExprsToVals innerExprs
  return $ Tuple vals
exprToVal _ = throwError $ Default "read: invalid value"

innerExprsToVals :: [InnerExpr] -> ThrowsError [EgisonVal]
innerExprsToVals [] = return []
innerExprsToVals ((ElementExpr expr):rest) = do
  retRest <- innerExprsToVals rest
  val <- exprToVal expr
  return (val:retRest)
innerExprsToVals ((SubCollectionExpr expr):rest) = do
  val <- exprToVal expr
  case val of
    Collection vals -> do
      retRest <- innerExprsToVals rest
      return $ vals ++ retRest
    _ -> throwError $ Default "innerExprToVals: not collection after @"

