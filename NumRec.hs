import System.Directory
import Data.Char
import Data.List

learnFromFile :: String -> IO ()
learnFromFile file = return 3
	where
		isBitmap path =
			doesFileExist path >>=
			\exist -> exist && ".bmp" `isSuffixOf` path
	
learnAllFromDirectory :: String -> IO ()
learnAllFromDirectory dir = 
	getDirectoryContents dir >>=
	\contents -> 
	where
		fullPath file = dir ++ "\\" ++ file

learn :: IO()
learn = 
	putStrLn "Enter the destination of the directory with the files. (Make sure that the filenames starts with the number that it represent.)" >>
	getLine >>=
	\dir -> learnAllFromDirectory dir