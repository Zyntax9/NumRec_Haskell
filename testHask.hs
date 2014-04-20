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
rgbVal val high =
	chr (125 + (div 130 (high * (read val :: Int))))
-- Converts list into ByteString
totalBS dat =
	CH.pack dat
	
	
test = genRGBA 9 >>= \rgba -> return (length rgba)
_test = loadData >>= \dat -> return (length (words (dat !! 0)))