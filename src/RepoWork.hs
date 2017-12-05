module RepoWork where

import System.IO
import System.Process

gitClone :: String -> IO ()
gitClone repo = callProcess "git" ["clone", repo]

removeRepo :: String -> IO ()
removeRepo repo = do
    callProcess "rm" ["-rf", repo]
    putStrLn $ repo ++ " removed."

