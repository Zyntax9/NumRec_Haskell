import System.Directory
import Data.Char
import Data.List
import qualified Data.ByteString as BS
import Data.Word
import Codec.BMP

splitRGBA :: [a] -> [(a,a,a,a)] -> [(a,a,a,a)]
splitRGBA pixels rgbaList
	| length pixels >= 4 = splitRGBA (removeFirstFour pixels) (rgbaList ++ [(pixels !! 0, pixels !! 1, pixels !! 2, pixels !! 3)])
	| otherwise			 = rgbaList
	where
		removeFirstFour :: [a] -> [a]
		removeFirstFour (_:_:_:_:val) = val

learnFromFile :: String -> IO ()
learnFromFile file = 
	isBitMap file >>=
		\isBMP ->
			if isBMP then
				putStrLn ("Learning from file: " ++ file) >>
				return ()
			else
				putStrLn (file ++ " was not a useful Bitmap file.") >>
			return ()
	where
		isBitMap path =
			doesFileExist path >>=
			\exist -> return $ exist && ".bmp" `isSuffixOf` path && (isDigit $ path !! ((length path) - 4))
		isDATAFile path =
			doesFileExist path >>=
			\exist -> return exist
		loadData path =
			isDATAFile path >>=
			\isDATA ->
				if isDATA then
					readFile path >>=
					\fileData -> return . lines $ fileData
				else
					return ["","","","","","","","","",""]
		saveData path dat =
			return . writeFile $ path (unlines $ dat) 
	
learnAllFromDirectory :: String -> IO ()
learnAllFromDirectory dir = 
	getDirectoryContents dir >>=
	\contents -> mapM_ (learnFromFile . fullPath) contents
	where
		fullPath file = dir ++ "\\" ++ file

learn :: IO()
learn = 
	putStrLn "Enter the destination of the directory with the files. (Make sure that the filenames starts with the number that it represent.)" >>
	getLine >>=
	\dir -> learnAllFromDirectory dir