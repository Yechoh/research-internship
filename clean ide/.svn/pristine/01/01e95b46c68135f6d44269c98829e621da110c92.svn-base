implementation module PmDirCache

// Search paths for a file

import StdArray, StdBool, StdEnum, StdList, StdOrdList
import StdFile, StdMaybe
import Directory
import UtilStrictLists, PmPath, UtilIO
import Platform


:: DirCache :== {(String,String,DateTime)} // module name, module path, module modified

emptyDateTime :== ({year`=0,month`=0,day`=0,dayNr`=0},{hours`=0,minutes`=0,seconds`=0})

:: Warn = Warn String String [(String,String,DateTime)]

DC_Setup :: !(List Pathname) !*Files -> (!(![String],![Warn],!.DirCache),!*Files)
DC_Setup paths files
	# paths					= RemoveDup paths
	# ((err,cache),files)	= setup paths files
	# cache					= sortBy (\(l,_,_) (r,_,_) -> l<r) cache
	// must remove duplicates otherwise binsearch gives errors...
	# (wrn,cache)			= removedups cache
	= ((err,wrn,{dir \\ dir <- cache}),files)
where
	setup Nil files
		= (([],[]),files)
	setup (path:!paths) files
		# path = winfix path							// local hack to fix dir names
		# ((ok,path`),files)		= pd_StringToPath path files
		| not ok
			# ((errs,cache),files)	= setup paths files
			= (([path:errs],cache),files)
		# ((err,dir),files)			= getDirectoryContents path` files
		| err <> NoDirError
			# ((errs,cache),files)	= setup paths files
			= (([path:errs],cache),files)
		# dir						= map getinfo dir		// only need common fileinfo...
		# dir						= filter (\(b,_,_)->not b) dir
		# dir						= map (\(_,n,m)->(n,path,m)) dir
		# ((errs,cache),files)		= setup paths files
		# cache						= dir ++ cache
		= ((errs,cache),files)

	//removedups :: !.[(String,String,DateTime)] -> .[(String,String,DateTime)]
	removedups [x=:(l,p,_):xs]
		# (wrn,xs)					= dropWarn (\(r,_,_) -> l==r) xs
		| isEmpty wrn
			# (wrn,xs)				= removedups xs
			= (wrn,[x:xs])
		# (wrn`,xs)					= removedups xs
		= ([Warn l p wrn:wrn`],[x:xs])
	removedups _      = ([],[])

	//dropWarn :: (a -> .Bool) !u:[a] -> u:[a]
	dropWarn f cons=:[a:x]	| f a	# (wrn,x) = dropWarn f x
									= ([a:wrn],x)
									= ([],cons)
	dropWarn f []					= ([],[])
	
	//winfix :: renames root directories
	winfix s = PlatformDependant
		(/*Win*/ if (size s > 0 && s.[size s - 1] == ':') (s +++. "\\") (s))
		(/*Mac*/ if (findPos ':' s 0 == ~1) (s +++. ":") (s))

	findPos c s i
		| i >= size s = ~1
		| s.[i] == c = i
		= findPos c s (inc i)
		
getinfo {fileName,fileInfo=fi=:{pi_fileInfo=dummyname=:{lastModified,isDirectory}}}
	= (isDirectory,fileName,lastModified)

DC_Update :: !.(String,String,DateTime) !*DirCache -> *DirCache
DC_Update (n`,p`,m`) cache
	# (maxi,cache)	= usize cache
	= binsearch 0 maxi maxi cache
where
	binsearch :: !Int !Int !Int !*DirCache -> *DirCache
	binsearch left right max cache
		| left >= right
			# newcache			= createArray (inc max) (n`,p`,m`)
			# (newcache,cache)	= copy (dec right) newcache cache 0 0
			# (newcache,cache)	= copy (max - right) newcache cache (inc right) right
			= newcache
		# mid					= (left+right)/2
		# ((n,p,m),cache)		= uselect cache mid
		| n` == n
			= update cache mid (n`,p`,m`)
		| n` < n
			= binsearch left mid max cache
		// n` > n
			= binsearch (inc mid) right max cache
	
	copy :: !Int !*DirCache !*DirCache !Int !Int -> (!*DirCache, !*DirCache)
	copy num new old newbegin oldbegin
		| num <= 0	= (new,old)
		# (e,old)	= uselect old oldbegin
		# new		= update new newbegin e
		= copy (dec num) new old (inc newbegin) (inc oldbegin)

DC_Search :: !Modulename !*DirCache -> *(!Bool,!Pathname,!DateTime,!*DirCache)
DC_Search mod cache
	# (maxi,cache)	= usize cache
	= binsearch 0 maxi mod cache
where
	binsearch left right mod cache
		| left >= right
			= (False,"",emptyDateTime,cache)
		# mid				= (left+right)/2
		# ((n,p,m),cache)	= uselect cache mid
		| mod == n
			= (True,p,m,cache)
		| mod < n
			= binsearch left mid mod cache
		// mod > n
			= binsearch (inc mid) right mod cache

//--

SearchDisk :: !Bool !Modulename !(List Pathname) !*Files -> ((!Bool,!Pathname),!*Files)
SearchDisk def_and_imp modname dirs disk
	= SearchDisk2 def_and_imp modname modname2 dirs disk
where
	modname2	| not def_and_imp		= modname
				| IsDefPathname modname	= MakeImpPathname modname
										= MakeDefPathname modname
	SearchDisk2 :: !Bool !Modulename !Modulename !(List Pathname) !*Files -> ((!Bool,!Pathname),!*Files)
	SearchDisk2 def_and_imp modname1 modname2 Nil disk
		=  ((False, EmptyPathname),disk)
	SearchDisk2 def_and_imp modname1 modname2 (dir:!rest) disk
		#	dir_modname1				= MakeFullPathname dir modname1
		#	(exists,disk)				= FExists dir_modname1 disk
		| exists
			= ((True, dir_modname1),disk)
		| not def_and_imp
			= SearchDisk2 def_and_imp modname1 modname2 rest disk
		#	dir_modname2				= MakeFullPathname dir modname2
		#	(exists,disk)				= FExists dir_modname2 disk
		| exists
			= ((True, dir_modname1),disk)
		= SearchDisk2 def_and_imp modname1 modname2 rest disk

//--

instance toString DirError
where
	toString NoDirError			= "NoDirError"
	toString DoesntExist 		= "DoesntExist"
	toString BadName 			= "BadName"
	toString NotEnoughSpace 	= "NotEnoughSpace"
	toString AlreadyExists 		= "AlreadyExists"
	toString NoPermission		= "NoPermission"
	toString MoveIntoOffspring 	= "MoveIntoOffspring"
	toString MoveAcrossDisks 	= "MoveAcrossDisks"
	toString NotYetRemovable 	= "NotYetRemovable"
	toString OtherDirError		= "OtherDirError"
