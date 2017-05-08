definition module directoryBrowsing

import iTasks
import System.File
import ideStores 

:: DirectoryType	= Cashed !Directory | NotCashed !FilePath !(FileName -> Bool) 

derive class iTask ChoiceNode
derive class iTask FileInfo, Tm

// from given directory path recursively fetch all files which name maches the predicate 

fetchDirectories 	::  !FilePath !(FileName -> Bool) -> Task Directory	

// search file in list of directories

findInDirectories 	:: !String ![Directory] -> Maybe FilePath
viewDirectory 		:: !String ![TaskCont FilePath (Task())] ![DirectoryType] -> Task ()

// directory browsing

askUserForFile		:: !FileName !(FileName -> Bool) -> Task (FilePath,FileName)
selectFromTreeLocal	:: !Bool !FilePath !(FileName -> Bool) -> Task (FilePath,String)		// fast: returns file selected satisfying pred
selectFromTree 		:: !Bool !FilePath !(FileName -> Bool) -> Task (FilePath,String) 		// slow: first searches given file system, returns file selected satisfying pred
browseDirectory 	:: !(FilePath -> Bool) -> Task ((FilePath,[FilePath]),Maybe FilePath) 	// returns directory, its subdirs, file slected

// 

getPwdName 			:: Task FilePath					// return present working directory name
isDirectory 		:: !FilePath -> Task Bool			// True if file is a directory
readDir 			:: !FilePath  -> Task [FileName]  	// returns all names of files and directories in given directory

// File I/O	
createFile 			:: !FilePath !FileName !String -> Task ()	// create new file in directory and store content
deleteThisFile 		:: !FilePath -> Task ()						// delete file
readFromFile 		:: !String -> Task (FileInfo,String)		// read file info and content
readLinesFromFile 	:: !String -> Task [String]					// read lines from file
writeToFile 		:: !String !String -> Task String			// write to file



