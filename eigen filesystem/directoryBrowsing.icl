implementation module directoryBrowsing

import iTasks

import iTasks.API.Extensions.Admin.WorkflowAdmin
import iTasks.UI.Layout, iTasks.UI.Definition, iTasks.UI.Editor.Builtin
import iTasks.API.Extensions.Editors.Ace
import StdFile, System.File
import System.Directory
import System.FilePath
import Data.Error


:: Directory = Dir FileName [Directory] [FileName]
:: FileName :== String 									// File name without path, but with extension
//:: MaybeContent :== Maybe String

derive class iTask Directory
derive class iTask ChoiceNode
//derive class iTask Maybe

selectIcon pwd _ 	 = Nothing	// don't know where to store icons yet
selectIcon pwd "icl" = Just (pwd </> "WebPublic" </> "Clean.icl.ico")
selectIcon pwd "dcl" = Just (pwd </> "WebPublic" </> "Clean.dcl.ico")
selectIcon pwd "prj" = Just (pwd </> "WebPublic" </> "Clean.prj.ico")
selectIcon pwd "abc" = Just (pwd </> "WebPublic" </> "Clean.abc.ico")
selectIcon pwd _ 	 = Nothing


// Directory & file selection using a tree

selectFromTree :: FilePath (FileName -> Bool) -> Task FilePath
selectFromTree pwd isWantedFile
	=				fetchDirectories pwd isWantedFile
	>>= \dirs ->	withShared dirs (\dir -> (editSelectionWithShared "dirs" False (SelectInTree (\d -> fst (conv 0 d)) (\_ idx -> idx)) dir (\_ -> []) ))
	>&> \mbsel ->	viewSharedInformation "Selected: " [ViewAs (findSelected pwd (conv 0 dirs))] mbsel			
	>>= \mbsel -> 	return (findSelected pwd (conv 0 dirs) mbsel)

where
	edit dirs =	editSelection "dirs" False (SelectInTree toTree fromTree) dirs [0..n]
	where
		(toTree,n) 	 	= conv 0 dirs
		fromTree _ idx 	= idx

	conv :: Int Directory -> ([ChoiceNode],Int)
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
		# cf		= {id = i, label = f, icon = selectIcon pwd (takeExtension f), expanded = False, children = []}
		# (cfs,j) 	= convFiles (i+1) fs
		= ([cf:cfs],j) 

findSelected :: FilePath ([ChoiceNode],Int) (Maybe [Int]) -> String
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


// directory browsing


browseDirectory :: (FilePath -> Bool) -> Task ((FilePath,[FilePath]),Maybe FilePath)
browseDirectory pred
 	=									getPwdName
 	>>- \pwd -> 						selectFile pwd pred
	>>- \((pwd,paths),chosen)	->		return ((pwd,paths),chosen)
where
	selectFile :: FilePath (FilePath -> Bool) -> Task ((FilePath,[FilePath]),Maybe FilePath)
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

fetchDirectories ::  FilePath (FileName -> Bool) -> Task Directory
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
	>>= \dirs ->			return [dir:dirs]
	
selectDirs :: FilePath [FileName] (FileName -> Bool) -> Task ([FileName],[FileName])
selectDirs pwd paths pred = selDirs` paths ([],[]) 
where
	selDirs` [] (dirs,nodirs) 
		= return (drop 2 (reverse dirs),sort (filter pred nodirs))
	selDirs` [f:fs] (dirs,nodirs)
		=			isDirectory (pwd </> f)
		>>= \yes -> if yes (selDirs` fs ([f:dirs],nodirs))
						   (selDirs` fs (dirs,[f:nodirs]))

isDirectory :: FilePath -> Task Bool
isDirectory file = worldIO isDir
where
	isDir world
	# (res,world) 	=	getFileInfo file world
	| isError res	=	(Error ("Cannot get directory info from file: " +++ file), world) 
	= (Ok (fromOk res).directory,world)

readDir :: FilePath  -> Task [FileName]  // returns file names (no path)
readDir pwd  = worldIO readDir`
where
	readDir` world
	# (res,	world)		= readDirectory pwd world
	| isError res		= (Error ("Cannot read directory: " +++ pwd), world)
	# names				= fromOk res
	= (Ok names, world)		// includes "." and ".."

getPwdName :: Task FilePath
getPwdName
	=					worldIO getPwd`
where
	getPwd` world 	
	# (res, world)	= 	getCurrentDirectory world 
	| isError res	= 	(Error "Cannot open current directory", world)
	# pwd			= 	fromOk res
	= (Ok pwd, world)

createFile :: FilePath FileName String -> Task ()
createFile path name content = worldIO (create (path </> name) content)
where
	create filename content world
	# (ok,file,world)			= fopen filename FWriteText world
	| not ok					= (Error ("Cannot open file: " +++ filename),world)
	# file						= fwrites content file
	# (ok,world)				= fclose file world
	| not ok					= (Error ("Cannot open file: " +++ filename),world)
	= (Ok (),world)	
	
readFromFile :: String -> Task (Maybe String)
readFromFile path = worldIO (read path) 
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

writeToFile :: String String -> Task String
writeToFile path content = worldIO (write path content) 
where 
	write path content world
	# (ok,file,world)			= fopen path FWriteText world
	| not ok					= (Error ("Cannot open file: " +++ path), world)
	# file						= fwrites content file
	# (ok,world)				= fclose file world
	| not ok					= (Error ("Cannot close file: " +++ path) ,world)
	= (Ok content, world)



