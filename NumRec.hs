import System.Directory
import Data.Char
import Data.List
import Codec.BMP

learnFromFile :: String -> IO ()
learnFromFile file = return ()
	where
		isBitMap :: String -> IO Bool
		isBitMap path =
			doesFileExist path >>=
			\exist -> return $ exist && ".bmp" `isSuffixOf` path
	
learnAllFromDirectory :: String -> IO ()
learnAllFromDirectory dir = 
	getDirectoryContents dir >>=
	\contents -> return ()
	where
		fullPath file = dir ++ "\\" ++ file

learn :: IO()
learn = 
	putStrLn "Enter the destination of the directory with the files. (Make sure that the filenames starts with the number that it represent.)" >>
	getLine >>=
	\dir -> learnAllFromDirectory dir