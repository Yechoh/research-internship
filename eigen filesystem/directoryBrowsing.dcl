definition module directoryBrowsing

import iTasks

:: Directory = Dir FileName [Directory] [FileName]
:: FileName :== String 									// File name without path, but with extension
//:: MaybeContent :== Maybe String

derive class iTask Directory
derive class iTask ChoiceNode
//derive class iTask (Maybe String)

// from given directory path recursively fetch all files which name maches the predicate 

fetchDirectories 	::  FilePath (FileName -> Bool) -> Task Directory	

// directory browsing

selectFromTree 		:: FilePath (FileName -> Bool) -> Task FilePath
browseDirectory 	:: (FilePath -> Bool) -> Task ((FilePath,[FilePath]),Maybe FilePath)

// 

getPwdName 			:: Task FilePath					// return present working directory name
isDirectory 		:: FilePath -> Task Bool			// True if file is a directory
readDir 			:: FilePath  -> Task [FileName]  	// returns all names of files and directories in given directory

// File I/O	

createFile 			:: FilePath FileName String -> Task ()	// create new file in directory and store content
readFromFile 		:: String -> Task (Maybe String)			// read from file. returns Nothing if the file cannot be found.
writeToFile 		:: String String -> Task String			// write to file



