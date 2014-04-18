import System.Directory
import Data.Char
import Data.List
import Data.Char
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CH
import Data.Word
import Codec.BMP

-- Splits the data from a BMP into a list of quadruples of Red/Green/Blue/Alpha integers
splitRGBA :: String -> [(Int,Int,Int,Int)]
splitRGBA pixels = 
	splitRGBAh pixels []
		
splitRGBAh :: String -> [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)]
splitRGBAh pixels rgbaList
	| length pixels >= 4 = splitRGBAh restPixels (rgbaList ++ [(ord r, ord g, ord b, ord a)])
	| otherwise			 = rgbaList
	where
		(r:g:b:a:restPixels) = pixels 

-- Learns from a specific file
learnFromFile :: String -> IO ()
learnFromFile file = 
	isBitMap file >>=
		\isBMP ->
			if isBMP then
				putStrLn ("Learning from file: " ++ file ++ " as the number: " ++ [numberToLearn]) >>
				loadData >>=
				\fileData ->  collectNewData file fileData >>=
				\newDat -> saveData $ changeLineOfData (digitToInt numberToLearn) fileData newDat
			else
				putStrLn (file ++ " was not a useful Bitmap file.") >>
				return ()
	where
		-- The number the file will learn for
		numberToLearn = file !! ((length file) - 5)
		-- Checks if file exists and is a .bmp file
		isBitMap path =
			doesFileExist path >>=
			\exist -> return $ exist && (".bmp" `isSuffixOf` path || ".BMP" `isSuffixOf` path) && isDigit numberToLearn
		-- Get the data from BMP
		loadBitmap path =
			readBMP path >>=
			\bmpData -> 
				case bmpData of
				Right dat ->
					return . CH.unpack . unpackBMPToRGBA32 $ dat
				Left err ->
					putStrLn "*** An error has occured in the loading of the BMP. Make sure it is a Windows V3 Bitmap file. ***" >>
					return ""
		-- Checks if a given file exists. We call it, we know that it will be a data file, if it exists
		isDATAFile path =
			doesFileExist path >>=
			\exist -> return exist
		-- Loads data from a data file at a given path
		loadData =
			isDATAFile "l.data" >>=
			\isDATA ->
				if isDATA then
					readFile "l.data" >>=
					\fileData -> return . lines $ fileData
				else
					return ["","","","","","","","","",""]
		-- Save data to a given path
		saveData dat =
			writeFile "l.data" (unlines dat)
		-- Change a specific line in a list
		changeLineOfData line fullData changeData =
			take (line - 1) fullData ++ [changeData] ++ drop line fullData
		-- Zips the RGBA quadruples with the number from the data file
		zipInTouple rgba fileData 
			| length rgba > 0 = zipInToupleh rgba fileData []
			| otherwise 	  = []
		zipInToupleh (rgba:rest) fileData dataSoFar 
			| length rest == 0 = dataSoFar ++ [(rgba, find fileData (length dataSoFar))]
			| otherwise 	   = zipInToupleh rest fileData (dataSoFar ++ [(rgba, find fileData (length dataSoFar))])
		-- Find a given element in list and return 0 if there is no such element
		find fileData index
			| length fileData < (index + 1) = 0
			| otherwise 					= read (fileData !! index) :: Int
		-- Gets the number that the file teaches
		nameToNumber fullPath = 
			digitToInt (fullPath !! ((length fullPath) - 5))
		-- Checks if the given RGBA value would be enabled:
		rgbaEnabled (r,g,b,a)
			| r + g + b > 0 = True
			| otherwise 	= False
		-- Changes the data depending on if the RGBA is enabled
		changeData (rgba, number)
			| rgbaEnabled rgba = show $ number + 1
			| otherwise 	   = show $ number - 1
		-- Changes all data with changeData
		changeAllData dat =
			map changeData dat
		-- Collects all the new data in a list
		collectNewData path dat =
			do
				bmpDat <- loadBitmap file
				lineDat <- collectLineData path dat
				return . unwords . changeAllData $ zipInTouple (splitRGBA bmpDat) lineDat
		-- Collects line data of file
		collectLineData path dat =
			return . words $ dat !! (digitToInt numberToLearn)
		
-- Starts learning for all files in a given directory
learnAllFromDirectory :: String -> IO ()
learnAllFromDirectory dir = 
	getDirectoryContents dir >>=
	\contents -> mapM_ (learnFromFile . fullPath) contents
	where
		-- Returns the full path to a file
		fullPath file = dir ++ "\\" ++ file

-- Starts the learning
learn :: IO()
learn = 
	putStrLn "Enter the destination of the directory with the files. (Make sure that the filenames starts with the number that it represent.)" >>
	getLine >>=
	\dir -> learnAllFromDirectory dir
	