module Paths_egison where
import Data.Version

version :: Version
version = Version [2,1,10] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName filename = return ("/home/egi/Egison-src/" ++ filename)
