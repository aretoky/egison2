module Paths_egison where
import Data.Version

version :: Version
version = Version [2,3,9] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName filename = return ("G:/xyx/program/egison/egison2/egison2/hs-src/" ++ filename)
