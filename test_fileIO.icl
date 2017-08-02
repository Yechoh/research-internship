module test_fileIO

import iTasks
import iTasks.Extensions.Admin.WorkflowAdmin
import iTasks.UI.Layout, iTasks.UI.Definition, iTasks.UI.Editor.Builtin
import iTasks.Extensions.Editors.Ace
import StdFile, System.File
import System.Directory
import System.FilePath
import Data.Error
import StdArray
import System.File
import System.OS

:: Directory = Dir FileName [Directory] [FileName]
:: FileName :== String

derive class iTask Directory
derive class iTask ChoiceNode

/*Start world = startEngine (fetch
    "./"
    ["tabs.icl"]
    >>= (\a. viewInformation "" ["tabs.icl"] ("a",a)) >>| return ()) world*/

/*Start world = startEngine (selectDirs
    "./"
    //"/home/martin/Documents/clean-bundle-itasks-windows-x86-latest/research-internship/"
    []
    (\a.True)
    >>= (\a. viewInformation "" ["tabs.icl"] ("a",a)) >>| return ()) world*/

Start world = startEngine (fetchDirectories
    //"./"
    "/home/martin/Documents/clean-bundle-itasks-windows-x86-latest/research-internship/"
    (\a.True)
    >>= (\a. viewInformation "" [] ("a",a)) >>| return ()) world

/*Start world = startEngine (readDir
    //"./"
    "/home/martin/Documents/clean-bundle-itasks-windows-x86-latest/research-internship/"
    //(\a.True)
    >>= (\a. viewInformation "" [] ("a",a)) >>| return ()) world*/

/*Start world = startEngine (isDirectory "./"
    >>= (\a. viewInformation "" [] ("a",a)) >>|
    isDirectory "/home/martin/Documents/clean-bundle-itasks-windows-x86-latest/research-internship/"
    //>>= (\a. viewInformation "" [] ("a",a)) >>|
    //isDirectory "jemoeder" //WARNING: cannot get directory info from file: jemoeder
    >>= (\a. viewInformation "" [] ("a",a)) >>|
    isDirectory "./ace"
    >>= (\a. viewInformation "" [] ("a",a)) >>|
    isDirectory "./aant.txt"
    >>= (\a. viewInformation "" [] ("a",a))
    ) world*/


/*Start world = startEngine (readFromFile
    //".\tabs.icl" //faalt
    "./tabs.icl" //werkt
    //"jemoeder" //faalt
    //"/home/martin/Documents/clean-bundle-itasks-windows-x86-latest/research-internship/tabs.icl" //ongetest
    >>= (\a. viewInformation "" [] ("a",a)) >>| return ()) world*/

isDirectory :: FilePath -> Task Bool
isDirectory file = accWorldError isDir id
where
	isDir world
	# (res,world) 	=	getFileInfo file world
	| isError res	=	(Error ("Cannot get directory info from file: " +++ file), world)
	= (Ok (fromOk res).directory,world)

fetchDirectories ::  FilePath (FileName -> Bool) -> Task Directory
fetchDirectories pwd pred
	= 						readDir pwd
	>>= \fnames ->			viewInformation "" [] fnames
    >>|                     selectDirs pwd (filter (\a. a<> "." && a<> "..") fnames) pred
	>>= \(dnames,fnames) ->	fetch pwd dnames
	>>= \dirs -> 			return (Dir pwd dirs fnames)
where
  fetch pwd []
	= 						return []
  fetch pwd [d:ds]
	=						fetchDirectories (pwd </> d) pred
	>>= \dir ->				fetch pwd ds
    	>>= \dirs ->			return [dir:dirs]

selectDirs :: FilePath [FileName] (FileName -> Bool) -> Task ([FileName],[FileName])
selectDirs pwd paths pred = selDirs` paths ([],[])
where
	selDirs` [] (dirs,nodirs)
		= return (dirs,sort (filter pred nodirs))
	selDirs` [f:fs] (dirs,nodirs)
		=			isDirectory (pwd </> f)
		>>= \yes -> if yes (selDirs` fs ([f:dirs],nodirs))
						   (selDirs` fs (dirs,[f:nodirs]))

readDir :: FilePath  -> Task [String]  // returns file names (no path)
readDir pwd  = accWorldError readDir` id
where
	readDir` world
	# (res,	world)		= readDirectory pwd world
	| isError res		= (Error ("Cannot read directory: " +++ pwd), world)
	# names				= fromOk res
	= (Ok names, world)		// includes "." and ".."

readFromFile :: String -> Task (Maybe String)
readFromFile path = accWorldError (read path) id
where
	read path world
	# (ok,file,world)			= fopen path FReadData world
	| not ok					= (Ok Nothing, world)
	# (res,file)				= readAll file
	# (ok,world)				= fclose file world
	| not ok					= (Error ("Cannot close file: " +++ path), world)
    = case res of
        Error e                 = (Error ("Cannot read File:" +++ path), world)
        Ok content              = (Ok (Just content), world)
