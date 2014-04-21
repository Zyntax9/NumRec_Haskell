import System.Directory
import Data.Char
import Data.List
import Data.Char
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CH
import Data.Word
import Codec.BMP

-- ***** START OF GLOBALLY USED FUNCTION SECTION ***** --

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
		
-- Checks if the given RGBA value would be enabled. Since 255:255:255 would be white, then everything else would mean that the pixel was enabled. (Black on white)
rgbaEnabled (r,g,b,a)
	| r + g + b < 765 	= True
	| otherwise 		= False

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
			
-- Zips the RGBA quadruples with the number from the data file
zipInTouple rgba fileData 
	| length rgba > 0 = zipInToupleh rgba fileData []
	| otherwise 	  = []
	where
		zipInToupleh (rgba:rest) fileData dataSoFar 
			| length rest == 0 = dataSoFar ++ [(rgba, find fileData (length dataSoFar))]
			| otherwise 	   = zipInToupleh rest fileData (dataSoFar ++ [(rgba, find fileData (length dataSoFar))])
		-- Find a given element in list and return 0 if there is no such element
		find fileData index
			| length fileData < (index + 1) = 0
			| otherwise 					= read (fileData !! index) :: Int
			
-- ***** END OF GLOBALLY USED FUNCTION SECTION ***** --
-- ***** START OF LEARNING SECTION ***** --

-- Learns from a specific file
learnFromFile :: String -> IO ()
learnFromFile file = 
	isBitmap file >>=
		\isBMP ->
			if isBMP then
				putStrLn ("Learning from file: " ++ file ++ " as the number: " ++ [numberToLearn]) >>
				loadData >>=
				\fileData ->  collectNewData file fileData >>=
				\newDat -> saveData $ changeLineOfData ((digitToInt numberToLearn) + 1) fileData newDat
			else
				putStrLn (file ++ " was not a useful Bitmap file.") >>
				return ()
	where
		-- The number the file will learn for
		numberToLearn = file !! ((length file) - 5)
		-- Checks if file exists and is a .bmp file
		isBitmap path =
			doesFileExist path >>=
			\exist -> return $ exist && (".bmp" `isSuffixOf` path || ".BMP" `isSuffixOf` path) && isDigit numberToLearn
		-- Save data to a temp file followed by it overwriting l.data
		saveData dat =
			do
			writeFile "temp.data" (unlines dat)
			renameFile "temp.data" "l.data"
		-- Change a specific line in a list
		changeLineOfData line fullData changeData =
			take (line - 1) fullData ++ [changeData] ++ drop line fullData
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
	putStrLn "\nEnter the destination of the directory with the files. (Make sure that the filenames ends with the number that it represent, i.e. \"test1.bmp\".)\n" >>
	getLine >>=
	\dir -> learnAllFromDirectory dir
	
-- ***** END OF LEARNING SECTION ***** --
-- ***** START OF GUESSING SECTION ***** --

-- Handles all voting of pixels
voting :: String -> IO [(Int, Int)]
voting path =
	do
	rgbaDat <- getRGBA
	dataDat <- getData
	collectData rgbaDat dataDat
	where
		-- Splits the information from the l.data file
		getData =
			loadData >>=
			\dat -> return (map words dat)
		-- Loads and splits the RGBA data from file
		getRGBA =
			loadBitmap path >>=
			\bmpDat -> return . splitRGBA $ bmpDat
		-- Collects all voting results
		collectData rgba dat = return $ collectDatah rgba dat 0 []
		collectDatah rgba dat count dataSoFar
			| (count + 1) >= length dat	= dataSoFar ++ [(count, collectLineData (dat !! count) rgba 0)]
			| otherwise					= collectDatah rgba dat (count + 1) (dataSoFar ++ [(count, collectLineData (dat !! count) rgba 0)])
		-- Collects all voting for a specific number
		collectLineData (word:restW) (rgba:restRGBA) val
			| (length restW == 0) || (length restRGBA == 0)	= val + (rgbaAnalyze rgba word)
			| otherwise										= collectLineData restW restRGBA (val + (rgbaAnalyze rgba word))
		-- Analyze the RGBA values
		rgbaAnalyze rgba nr
			| (rgbaEnabled rgba) && ((read nr :: Int) > 0)			= 1
			| (not . rgbaEnabled $ rgba) && ((read nr :: Int) < 0)	= 1
			| otherwise												= 0

guessFile :: String -> IO()
guessFile path =
	isDATAFile "l.data" >>=
	\datExist ->
		if datExist then
			isBitmap path >>=
			\bmpExists -> 
				if bmpExists then
					putStrLn "\nAnalyzing..." >>
					collectedData >>=
						\cDat -> putStrLn cDat
				else
					putStrLn "\nThe Bitmap file was not useable or does not exist.\n" >>
					return ()
		else
			putStrLn "\nThere is not data from previously learned files.\n" >>
			return ()
	where
		-- Collected data
		collectedData = 
			voting path >>=
			\votingVal ->
				return (rankingIndexSort . sortList $ votingVal)
		-- Checks if the given file is a bitmap file
		isBitmap path =
			doesFileExist path >>=
			\exist -> return $ exist && (".bmp" `isSuffixOf` path || ".BMP" `isSuffixOf` path)
		-- Returns a string containing all guesses
		rankingIndexSort scores = rankingIndexSorth scores [] 0
		rankingIndexSorth [] dataSoFar _ = dataSoFar
		rankingIndexSorth ((a,b):rest) dataSoFar count
			| count == 0 	= rankingIndexSorth rest (dataSoFar ++ "\n1st: " ++ (show a) ++ " with " ++ (show b) ++ " votes.\n") (count + 1)
			| count == 1	= rankingIndexSorth rest (dataSoFar ++ "2nd: " ++ (show a) ++ " with " ++ (show b) ++ " votes.\n") (count + 1)
			| count > 1 	= rankingIndexSorth rest (dataSoFar ++ (show (count + 1)) ++ "th: " ++ (show a) ++ " with " ++ (show b) ++ " votes.\n") (count + 1)
		-- Gets the sorted list
		sortList listDat = sortedList listDat []
		sortedList :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
		sortedList ((index,val):rest) dataSoFar
			| null rest	= findSpot (index,val) dataSoFar
			| otherwise	= sortedList rest (findSpot (index,val) dataSoFar)
		-- Finds a spot on the sorted list	
		findSpot dat listDat = findSpoth dat listDat 0
		findSpoth (index, val) listDat count
			| count >= length listDat				= pushAt count listDat (index, val)
			| val > rightElement (listDat !! count)	= pushAt count listDat (index, val)
			| otherwise								= findSpoth (index, val) listDat (count + 1)
		-- Removes element at index
		removeAt index listDat
			| index == 0				= tail listDat
			| index >= length listDat	= init listDat
			| otherwise					= (take index listDat) ++ (drop (index + 1) listDat)
		-- Pushes an element at index
		pushAt index listDat dat
			| index == 0				= [dat] ++ listDat
			| index >= length listDat	= listDat ++ [dat]
			| otherwise					= (take index listDat) ++ [dat] ++ (drop index listDat)
		-- returns the right element of a tuple
		rightElement (left, right) = right
		
-- Starts the guessing
guess :: IO ()
guess =
	putStrLn "\nEnter the complete path of the file you would like me to guess." >>
	getLine >>=
	\cpath -> guessFile cpath
	
-- ***** END OF GUESSING SECTION ***** --
-- ***** START OF INFO SECTION ***** --

info :: IO ()
info =
	readFile "info.txt" >>=
	\dat -> putStrLn dat >>
	return ()
	
-- ***** END OF INFO SECTION ***** --
-- ***** START OF ILLUSTRATION SECTION ***** --

illustration :: IO ()
illustration =
	isDATAFile "l.data" >>=
	\exist ->
		if exist then
			putStrLn "\nEnter the numer you would like to illustrate in a bitmap." >>
			getLine >>=
			\nr ->
				if(((read nr :: Int) < 10) && ((read nr :: Int) > -1)) then
					genRGBA (read nr :: Int) >>=
					\rgba -> saveBitmap rgba (read nr :: Int)
				else
					putStrLn "\nThe number must be 0-9.\n"
		else
			putStrLn "\nThere is no data to illustrate from.\n"
	where
		-- Saves data as bitmap
		saveBitmap rgba nr =
			writeBMP ("Illustration\\" ++ (show nr) ++ ".bmp") (packRGBA32ToBMP 25 40 (totalBS rgba))
		-- Generates the RGBA data
		genRGBA nr = 
			loadData >>=
			\dat -> return $ genRGBAh (words (dat !! nr)) (highest (words (dat !! nr))) []
		genRGBAh (a:rest) high dataSoFar
			| null rest	= dataSoFar ++ [rgbVal a high, rgbVal a high, rgbVal a high, chr 255]
			| otherwise	= genRGBAh rest high (dataSoFar ++ [rgbVal a high, rgbVal a high, rgbVal a high, chr 255])
		-- Returns the highest element in list
		highest dat = highesth dat 0
		highesth [] high = high
		highesth (start:rest) high
			| (read start :: Int) > high	= highesth rest (read start :: Int)
			| otherwise						= highesth rest high
		-- Returns a color value for the RGB
		rgbVal val high 
			| (read val :: Int) <= 0	= chr 255
			| otherwise					= chr (130 - ((div 130 high) * (read val :: Int)))
		-- Converts list into ByteString
		totalBS dat =
			CH.pack dat
		
-- ***** END OF ILLUSTRATION SECTION ***** --
-- ***** START OF MAIN SECTION ***** --

main :: IO ()
main =
	do
		putStrLn "\nPlease enter the the command for the action you would like to do.\nThe commands are:\n - learn\n - guess\n - info\n - illustrate\n"
		cmd <- getLine
		command cmd
		main
	where
		command cmd
			| cmd == "learn"		= learn
			| cmd == "guess"		= guess
			| cmd == "info"			= info
			| cmd == "illustrate"	= illustration
			| otherwise				= putStrLn "The command was not recognized."
	
-- ***** END OF MAIN SECTION ***** --