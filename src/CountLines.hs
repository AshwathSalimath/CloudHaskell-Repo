-- Count lines of code
-- Usage:
-- CountLines file_path           : Count the lines of the file specified
-- CountLines directory_path ext  : Count the lines of the files whose names are suffixed with ext under the directory specified recursively

module CountLines where
import System.Environment
import System.IO
import System.Directory
import Control.Monad
import Data.List

-- main = do
--     arg <- getArgs
--     n <- countLines arg
--     when (n >= 0) $ putStrLn $ show n

countLines :: String -> IO Int
countLines filename = countLinesInFile filename
-- countLines [path, ext] = countLinesInDirectory path ext
-- countLines [] = do
--     putStrLn "Usage:"
--     putStrLn "CountLines file_path           : Count the lines of the file specified"
--     putStrLn "CountLines directory_path ext  : Count the lines of the files whose names are suffixed with ext under the directory specified recursively"
--     return (-1)
-- countLines _ = do
--     putStrLn "Invalid parameters!"
--     return (-1)


countLinesInFile :: String -> IO Int
countLinesInFile filename = do
    content <- readFile filename
    return $ length $ lines content


-- countLinesInDirectory :: String -> String -> IO Int
-- countLinesInDirectory path ext = do
--     filelist <- getDirectoryContents path
--         >>= filterM (\name -> return $ name /= ".." && name /= ".")
--         >>= mapM (return . (path_++))
--         >>= filterM (isFileType ext)
--     mapM_ putStrLn filelist
--     lineCounts <- mapM count filelist
--     return $ sum lineCounts
--     where
--         isFileType :: String -> String -> IO Bool
--         isFileType ext name = do
--             b <- doesFileExist name
--             if b
--                 then return $ isSuffixOf ext name
--                 else return True
--         count name = do
--             b <- doesFileExist name
--             if b
--                 then countLinesInFile name
--                 else countLinesInDirectory name ext
--         path_ = path ++ "/"

