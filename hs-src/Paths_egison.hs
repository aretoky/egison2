module Paths_egison where
import Data.Version

version :: Version
version = Version [2,3.0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName filename = return ("/home/egi/egison2/" ++ filename)
