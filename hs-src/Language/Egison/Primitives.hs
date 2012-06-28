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
makePort mode [String filename] = liftM (Port filename) $ liftIO $ openFile filename mode
makePort _ [] = throwError $ NumArgs 1 []
makePort _ args@(_ : _) = throwError $ NumArgs 1 args

closePort :: [EgisonVal] -> IOThrowsError EgisonVal
closePort [Port _ port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

--writeChar :: [EgisonVal] -> IOThrowsError EgisonVal
--writeChar 