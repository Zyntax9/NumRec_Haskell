import System.Directory

learnAllFromDirectory :: String -> IO ()
learnAllFromDirectory dir = 
	getDirectoryContents dir >>=
	
	where
		
	

learn :: IO()
learn = 
	putStrLine "Enter the destination of the directory with the files. (Make sure that the filenames starts with the number that it represent.)" >>
	getLine >>=
	learnAllFromDirectory