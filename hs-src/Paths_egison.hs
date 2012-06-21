module Paths_egison where

getDataFileName :: FilePath -> IO FilePath
getDataFileName filename = return ("/home/egi/Egison-src/lib/" ++ filename)
