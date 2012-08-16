module Language.Egison.Numerical where
import Language.Egison.Types
import Control.Applicative
import Control.Monad.Error
import Data.Array

singleOp :: (PrimitiveVal a, PrimitiveVal b) => (a -> b) -> [EgisonVal] -> ThrowsError EgisonVal
singleOp op [x] = toEgisonVal . op <$> fromEgisonVal x
singleOp _ params = throwError $ NumArgs 1 params

multiOp :: PrimitiveVal a => (a -> a -> a) -> [EgisonVal] -> ThrowsError EgisonVal
multiOp _ emptyList@[] = throwError $ NumArgs 2 emptyList
multiOp _ singleVal@[_] = throwError $ NumArgs 2 singleVal
multiOp op aparams = toEgisonVal . foldl1 op <$> mapM fromEgisonVal aparams

binaryOp :: (PrimitiveVal a, PrimitiveVal b, PrimitiveVal c) => (a -> b -> c) -> [EgisonVal] -> ThrowsError EgisonVal
binaryOp _ emptyList@[] = throwError $ NumArgs 2 emptyList
binaryOp _ singleVal@[_] = throwError $ NumArgs 2 singleVal
binaryOp op [a, b] = toEgisonVal <$> liftM2 op (fromEgisonVal a) (fromEgisonVal b)

-- |Convert a number to a string; radix is optional, defaults to base 10
--numToString :: [EgisonVal] -> IOThrowsError EgisonVal
--numToString [(Number n), (Number radix)] = do
--  case radix of
--    2 -> do -- Nice tip from StackOverflow question #1959715
--             liftIO $ stringToCharCollection $ showIntAtBase 2 intToDigit n ""
--    8 -> liftIO $ stringToCharCollection $ printf "%o" n
--    10 -> liftIO $ stringToCharCollection $ printf "%d" n
--    16 -> liftIO $ stringToCharCollection $ printf "%x" n
--    _ -> throwError $ BadSpecialForm "Invalid radix value" $ Number radix

-- |Convert a float to a string; radix is optional, defaults to base 10
--floatToString :: [EgisonVal] -> IOThrowsError EgisonVal
--floatToString [(Float n)] = liftIO $ stringToCharCollection $ show n
--floatToString [x] = throwError $ TypeMismatch "number" x
--floatToString badArgList = throwError $ NumArgs 1 badArgList

isEgisonEOF :: [EgisonVal] -> ThrowsError EgisonVal
isEgisonEOF [EOF] = return $ Bool True
isEgisonEOF [_] = return $ Bool False
isEgisonEOF badArgList = throwError $ NumArgs 1 badArgList

-- - end Numeric operations section

stringToChars :: [EgisonVal] -> ThrowsError EgisonVal
stringToChars [(String str)] = return $ Collection $ map Char str
stringToChars [x] = throwError $ TypeMismatch "string" [x]
stringToChars badArgList = throwError $ NumArgs 1 badArgList

charsToString :: [EgisonVal] -> ThrowsError EgisonVal
charsToString [(Collection chars)] = String <$> mapM fromEgisonVal chars
charsToString [x] = throwError $ TypeMismatch "collection of chars" [x]
charsToString badArgList = throwError $ NumArgs 1 badArgList

---------------------------------------------------
-- Array
---------------------------------------------------

arrayDimension :: [EgisonVal] -> ThrowsError EgisonVal
arrayDimension [(Array d _ _)] = return $ Number d
arrayDimension [x] = throwError $ TypeMismatch "array" [x]
arrayDimension badArgList = throwError $ NumArgs 1 badArgList

arrayRange :: [EgisonVal] -> ThrowsError EgisonVal
arrayRange [(Array _ ns _)] = return $ Tuple $ map Number ns
arrayRange [x] = throwError $ TypeMismatch "array" [x]
arrayRange badArgList = throwError $ NumArgs 1 badArgList

arraySize :: [EgisonVal] -> ThrowsError EgisonVal
arraySize [(Number m), (Array _ ns _)] = return $ Number $ nth m ns
arraySize [x, y] = throwError $ TypeMismatch "number, array" [x, y]
arraySize badArgList = throwError $ NumArgs 2 badArgList

arrayKeys :: [EgisonVal] -> ThrowsError EgisonVal
arrayKeys [(Array _ ms _)] = return $ Collection $ map (Tuple . map Number) $ indexList ms
arrayKeys [x] = throwError $ TypeMismatch "array" [x]
arrayKeys badArgList = throwError $ NumArgs 1 badArgList

arrayIsRange :: [EgisonVal] -> ThrowsError EgisonVal
arrayIsRange [(Tuple key), (Array _ ms _)] = Bool . and . zipWith (\m n -> n > 0 && n <= m) ms <$> mapM fromEgisonVal key
arrayIsRange [x, y] = throwError $ TypeMismatch "key, array" [x, y]
arrayIsRange badArgList = throwError $ NumArgs 2 badArgList

arrayRef :: [EgisonVal] -> ThrowsError EgisonVal
arrayRef [(Tuple nums), (Array _ ms arr)] = do
  i <- integersToInteger ms <$> mapM fromEgisonVal nums
  return $ (arr ! i)
arrayRef [x, y] = throwError $ TypeMismatch "tuple of number, array" [x, y]
arrayRef badArgList = throwError $ NumArgs 2 badArgList