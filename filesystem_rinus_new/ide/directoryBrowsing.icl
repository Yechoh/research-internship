implementation module directoryBrowsing

import iTasks

import iTasks.API.Extensions.Admin.WorkflowAdmin
import iTasks.UI.Layout, iTasks.UI.Definition, iTasks.UI.Editor.Builtin
import iTasks.API.Extensions.Editors.Ace
import StdFile, System.File
import System.Directory
import System.FilePath
import Data.Error
import StdArray

import System.File

import ideStores 


derive class iTask ChoiceNode
derive class iTask FileInfo, Tm

selectIcon  _ 	 = Nothing			// ??? Somehow these icons are not found, or they are of the wrong format...
selectIcon  "icl" = Just ("Clean.icl.ico")
selectIcon  "dcl" = Just ("Clean.dcl.ico")
selectIcon  "prj" = Just ("Clean.prj.ico")
selectIcon  "abc" = Just ("Clean.abc.ico")
selectIcon  _ 	 = Nothing

viewDirectory :: !String ![TaskCont FilePath (Task())] ![DirectoryType] -> Task ()
viewDirectory prompt taskCont dirs 
	= forever
	(			anyTask [showDir dir \\ dir <- dirs]
	>>*			taskCont @! ()
	) 
where	
	showDir (Cashed dir=:(Dir name _ _))
		= 		editSelection () False (SelectInTree (\d -> fst (conv 0 d)) (\_ idx -> idx)) dir [] @  
							(\mbsel -> findSelected name (conv 0 dir) (Just mbsel))
	showDir (NotCashed name pred)
		= 			fetchDirectories name pred
		>>- \dir ->	editSelection () True (SelectInTree (\d -> fst (conv 0 d)) (\_ idx -> idx)) dir [] @  
							(\mbsel -> findSelected name (conv 0 dir) (Just mbsel))

// Directory & file selection using a tree

askUserForFile:: !FileName !(FileName -> Bool) -> Task (FilePath,FileName)
askUserForFile file isWantedFile
	=						viewInformation ("Where is " +++ file +++ "?") [] ""
		||-					getPwdName
 		>>- \pwd ->			selectFromTreeLocal True pwd isWantedFile

selectFromTreeLocal :: !Bool !FilePath !(FileName -> Bool) -> Task (FilePath,String)
selectFromTreeLocal show fp isWantedFile
	=						readDir fp
	>>= \all ->				selectDirs fp all isWantedFile
	>>= \(dnames,fnames) -> let initDir = Dir (dropDirectory fp) [Dir sdir [] [] \\ sdir <- dnames] fnames
							in withShared initDir (browse fp initDir) 
where
	browse cursel dirs sdirs
		=			edit cursel dirs sdirs
		>>* 		[ OnAction (Action "Found")	 (ifValue (\mbsel -> isWantedFile (snd (split fp dirs mbsel))) (\mbsel -> return (split fp dirs mbsel)))
					, OnAction (Action "Open") (ifValue (\mbsel -> cursel <> selected mbsel dirs &&
																	 not (isWantedFile (snd (split fp dirs mbsel))))  (\mbsel -> selectFromTreeLocal show (selected mbsel dirs) isWantedFile))
					, OnAction (Action "Up")	 (always (selectFromTreeLocal show (takeDirectory fp) isWantedFile))
					]

	edit cursel dirs sdirs
		= 				editSelectionWithShared () False (SelectInTree (\d -> fst (conv 0 d)) (\_ idx -> idx)) sdirs (\_ -> [])
		>&> \mbsel ->	(if show (viewSharedInformation () [ViewAs (findSelected fp (conv 0 dirs))] mbsel)
								(get mbsel))

	selected mbsel dirs = findSelected fp (conv 0 dirs) mbsel



selectFromTree :: !Bool !FilePath !(FileName -> Bool) -> Task (FilePath,String)
selectFromTree show fp isWantedFile
	=				fetchDirectories fp isWantedFile
	>>= \dirs ->	withShared dirs (edit dirs)
		>>* 		[ OnAction (Action "Select")	(ifValue (\mbsel -> isWantedFile (snd (split fp dirs mbsel))) (\mbsel -> return (split fp dirs mbsel)))
					, OnAction (Action "Up")		(ifCond (snd (splitFileName fp) <> "") (selectFromTree show (takeDirectory fp) isWantedFile))
					]
where

	edit dirs sdirs
		= 				editSelectionWithShared () False (SelectInTree (\d -> fst (conv 0 d)) (\_ idx -> idx)) sdirs (\_ -> [])
		>&> \mbsel ->	if show (viewSharedInformation "Selected: " [ViewAs (findSelected fp (conv 0 dirs))] mbsel) 
								(get mbsel)		

split fp dirs mbsel
	= (takeDirectory selected, dropDirectory selected)
where
	selected = findSelected fp (conv 0 dirs) mbsel

conv :: !Int !Directory -> ([ChoiceNode],Int)
conv i (Dir pwd dirs files) = convDirs i pwd dirs files
where
	convDirs i pwd dirs files
	# (cds,ii)	= convAllDirs (i+1) dirs
	# (cfs,iii)	= convFiles ii files
	= ([{id = i, label = dropDirectory pwd, icon = Nothing, expanded = False, children = cds ++ cfs}],iii)

	convAllDirs	i [] = ([],i)
	convAllDirs i [Dir pwd dirs fils:ds]
	# (cd,ii) 	= convDirs i pwd dirs fils
	# (cds,iii)	= convAllDirs ii ds
	= (cd++cds,iii)

	convFiles i [] 
				= ([],i)
	convFiles i [f:fs]
	# cf		= {id = i, label = f, icon = selectIcon (takeExtension f), expanded = False, children = []}
	# (cfs,j) 	= convFiles (i+1) fs
	= ([cf:cfs],j) 

fetchDirectories ::  !FilePath !(FileName -> Bool) -> Task Directory
fetchDirectories pwd pred
	= 						readDir pwd
	>>= \fnames ->			selectDirs pwd fnames pred
	>>= \(dnames,fnames) ->	fetch pwd dnames
	>>= \dirs -> 			return (Dir pwd dirs fnames)
where
  fetch pwd []  
	= 						return []
  fetch pwd [d:ds]   
	=						fetchDirectories (pwd </> d) pred
	>>= \dir ->				fetch pwd ds 
	>>= \dirs ->			case dir of
								(Dir _ [] []) -> return dirs
								_ -> return [dir:dirs]

findSelected :: !FilePath !([ChoiceNode],Int) (Maybe [Int]) -> String
findSelected pwd (nodes,i) Nothing  =  pwd
findSelected pwd (nodes,i) (Just idx)
| idx == [] 				= pwd
| i < hd idx 				= pwd
| hd idx == 0				= pwd
= travers (takeDirectory pwd) nodes (hd idx)
where
	travers pwd [] _			= pwd
	travers pwd [n1] i
	| n1.ChoiceNode.id == i 	= pwd </> n1.ChoiceNode.label
	| otherwise					= travers (pwd </> n1.ChoiceNode.label) n1.ChoiceNode.children i
	travers pwd [n1,n2:ns] i 
	| i >= n2.ChoiceNode.id 	= travers pwd [n2:ns] i
	| otherwise					= travers pwd [n1] i

findInDirectories :: !String ![Directory] -> Maybe FilePath
findInDirectories fileName [] 
	= Nothing
findInDirectories fileName [Dir pwd subdir files:dirs] 
	= if (isMember fileName files) 
			(Just pwd)
			(case (findInDirectories fileName subdir) of
				Nothing -> findInDirectories fileName dirs
				found   -> found
			)

// directory browsing

browseDirectory :: !(FilePath -> Bool) -> Task ((FilePath,[FilePath]),Maybe FilePath)
browseDirectory pred
 	=									getPwdName
 	>>- \pwd -> 						selectFile pwd pred
	>>- \((pwd,paths),chosen)	->		return ((pwd,paths),chosen)
where
	selectFile :: !FilePath !(FilePath -> Bool) -> Task ((FilePath,[FilePath]),Maybe FilePath)
	selectFile pwd pred
	 	=					readDir pwd
		>>- \all ->			selectDirs pwd all pred // only show wanted files 
		>>- \(dirs,files) ->
		let all = dirs ++ files in		
		(					enterChoice  pwd [ChooseFromGrid id] all 
		>>*					[ OnAction (Action "Up") (always (selectFile (takeDirectory pwd) pred))	
							, OnAction  ActionCancel (always (return ((pwd,all),Nothing)))
							, OnAction  ActionOpen   (hasValue (continue pwd dirs files pred))
							, OnAction  ActionNew    (always (createNewFile pwd >>| selectFile pwd pred))
		 					]
		)
	where
	 continue pwd dirs files pred chosen
		= if (isMember chosen dirs)  	
				(selectFile (pwd </> chosen) pred)
				(return ((pwd,dirs ++ files),Just chosen))
				
	 createNewFile pwd 
	 	= 				updateInformation "Give new file name: " [] "name.icl"
	 	>>= \name ->	createFile pwd name ""

	
selectDirs :: !FilePath ![FileName] !(FileName -> Bool) -> Task ([FileName],[FileName])
selectDirs pwd paths pred = selDirs` paths ([],[]) 
where
	selDirs` [] (dirs,nodirs) 
		= return (reverse dirs,sort (filter pred nodirs))
	selDirs` [f:fs] (dirs,nodirs)
		=			isDirectory (pwd </> f)
		>>= \yes -> if yes (selDirs` fs ([f:dirs],nodirs))
						   (selDirs` fs (dirs,[f:nodirs]))

isDirectory :: !FilePath -> Task Bool
isDirectory file = worldIO isDir
where
	isDir world
	# (res,world) 	=	getFileInfo file world
	| isError res	=	(Error ("Cannot get directory info from file: " +++ file), world) 
	= (Ok (fromOk res).directory,world)

readDir :: !FilePath  -> Task [FileName]  // returns file names (no path)
readDir pwd  = worldIO readDir`
where
	readDir` world
	# (res,	world)		= readDirectory pwd world
	| isError res		= (Error ("Cannot read directory: " +++ pwd), world)
	# names				= fromOk res
	= (Ok [name \\ name <- names | name <> "." && name <> ".."], world)		// do not include "." and ".."

getPwdName :: Task FilePath
getPwdName
	=					worldIO getPwd`
where
	getPwd` world 	
	# (res, world)	= 	getCurrentDirectory world 
	| isError res	= 	(Error "Cannot open current directory", world)
	# pwd			= 	fromOk res
	= (Ok pwd, world)

createFile :: !FilePath !FileName !String -> Task ()
createFile path name content = worldIO (create (path </> name) content)
where
	create filename content world
	# (ok,file,world)			= fopen filename FWriteText world
	| not ok					= (Error ("Cannot open file: " +++ filename),world)
	# file						= fwrites content file
	# (ok,world)				= fclose file world
	| not ok					= (Error ("Cannot open file: " +++ filename),world)
	= (Ok (),world)	
	
deleteThisFile :: !FilePath -> Task ()
deleteThisFile fileName = worldIO deleteIt
where
	deleteIt world
	# (ok,world)				= deleteFile fileName world
	= case ok of
		(Error e)				= (Error ("Canot delete file " +++ fileName),world)
		_						= (Ok (),world)	


readFromFile :: !String -> Task (FileInfo,String)
readFromFile path = worldIO (read path) 
where 
	read path world
	# (ok,file,world)			= fopen path FReadData world
	| not ok					= (Error ("Cannot find file: " +++ path), world) 
	# (res,file)				= readAll file
	# (ok,world)				= fclose file world
	| not ok					= (Error ("Cannot close file: " +++ path), world)
	# (info,world)				= getFileInfo path world
    = case res of
        Error e                 = (Error ("Cannot read File: " +++ path), world)
        Ok content              =  case info of
        							Error _ -> (Error ("Cannot read infor from File: " +++ path), world)
        							Ok info -> (Ok (info,content), world)

readLinesFromFile :: !String -> Task [String]
readLinesFromFile path = worldIO (read path) 
where 
	read path world
	# (ok,file,world)			= fopen path FReadData world
	| not ok					= (Error ("Cannot find file: " +++ path), world) 
	# (res,file)				= readAllLines file []
	# (ok,world)				= fclose file world
	| not ok					= (Error ("Cannot close file: " +++ path), world)
    =  (Ok res, world)

	readAllLines file accu 
	# (line,file) 				= freadline file
	| line == ""				= (reverse accu,file)
	= readAllLines file [line:accu]

writeToFile :: !String !String -> Task String
writeToFile path content = worldIO (write path content) 
where 
	write path content world
	# (ok,file,world)			= fopen path FWriteText world
	| not ok					= (Error ("Cannot open file: " +++ path), world)
	# file						= fwrites content file
	# (ok,world)				= fclose file world
	| not ok					= (Error ("Cannot close file: " +++ path) ,world)
	= (Ok content, world)



