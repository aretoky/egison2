module Paths_egison where

version :: String
version = "x.x.x"

showVersion :: String -> String
showVersion v = v

getDataFileName :: FilePath -> IO FilePath
getDataFileName filename = return ("/home/egi/Egison-src/" ++ filename)
