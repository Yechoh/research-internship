implementation module ideEditor

import iTasks

import iTasks.API.Extensions.Admin.WorkflowAdmin
import iTasks.UI.Layout, iTasks.UI.Definition, iTasks.UI.Editor.Builtin
import iTasks.API.Extensions.Editors.Ace
import StdFile, System.File, StdArray
import System.Directory
import System.FilePath
import Data.Error

import createAndRunExec
import directoryBrowsing

import iTasks.API.Extensions.Admin.TonicAdmin
import iTasks._Framework.Tonic
import Text.HTML, Internet.HTTP
import System.Environment
import Text

import ideStores, ideUtil, ideConstants, projectOptions

// ------- Ace editor is used for editing files

initAce :: !Bool -> (!AceOptions,!AceState)
initAce readOnly  
	= ({defaultValue & AceOptions.mode = "mode-haskell"}, {defaultValue & disabled = readOnly, cursor = (0,0)})

fromAce :: !(!AceOptions,!AceState) -> String
fromAce (options,state)
	= (join "\n" state.lines)

updAceLine :: !String !(Shared (!AceOptions,!AceState)) -> Task ()
updAceLine content editorStore
	= upd (\(options,state) -> (options,{state & lines = split "\n" (convert_cr content)})) editorStore @! ()

convert_cr :: !String -> String
convert_cr str = {c \\ c <-: str | c <> '\r'}

// -------

editorPage :: PublishedTask
editorPage = publish editorPageName	(\_ -> editorPage`)
where
	editorPage`
		=				get parmStore
		>>- \parm ->   	get recentFiles
		>>- \recent ->	case parm of
							Nothing 					->	openRecent False
							Just (editable,fileName)	->		set Nothing parmStore 
															>>|	editFile editable fileName

jumpToEditorPage :: !Bool !Bool !FilePath -> Task ()
jumpToEditorPage newPage readOnly fileName 
	= 		jumpToBrowserEditor` readOnly fileName <<@ InModalDialog @! () 
where
	jumpToBrowserEditor` readOnly fileName
		=	set (Just (readOnly,fileName)) parmStore									// store parameter for to be read by page called
		>>|	mbCancel
				(		viewInformation "Click on link to open editor: " [] (hopToEditor fileName) 
				||-		wait () (\parm -> isNothing parm) parmStore @! ()				// wait until parameter has been read by called page
				)
		>>| return ()

	hopToEditor text = (ATag [HrefAttr "/editor", TargetAttr (if newPage "_blank" "_inline")] [Text text])

// -------

openRecent :: Bool -> Task ()
openRecent newPage
	=				get recentFiles
	>>- \recent ->	enterChoice "Select file to open..." [ChooseFromGrid (\(ro,file) -> (dropDirectory file, ro, file))] recent
	>>* 			[ OnAction (Action "Open") (hasValue (\r -> jumpToEditorPage newPage (fst r) (snd r)))
					, OnAction ActionCancel (always (return()))
					] 

// ------- jumping Temp fix, would be nice to jump to a page OnAction, and pass a parameter, index in SDS table to workon task with a certain taskId 

jumpToSetSettings 
	=	viewInformation () [] (jumpToPage  "Open Set Settings" "/set")  >>| return ()

jumpToCloogle
	=	viewInformation () [] (jumpToPage  "Open Cloogle" "https://cloogle.org/")  

jumpToPage text page  
	= (ATag [HrefAttr page, TargetAttr "_blank"] [Text text])


// -------

editFile :: !Bool !FileName -> Task ()
editFile readOnly fname 
	=		withShared (initAce readOnly) editFile`														// create share used for the Ace editor
where 
	editFile` :: !(Shared (!AceOptions,!AceState)) -> Task ()
	editFile` editorStore
		=	showEditor 
		-|| (forever (errorWindow fname editorStore))	<<@ ApplyLayout (arrangeWithSideBar 1 BottomSide 200 False)
	where
		showEditor :: Task ()
		showEditor 
			=	((showSettings <<@ ApplyLayout (setAttributes (sizeAttr (ExactSize 210) FlexSize)))
			-||	(fileEditor  fname)  <<@ ApplyLayout (arrangeWithSideBar 0 LeftSide 210 True))  
	
		fileEditor :: !String -> Task ()
		fileEditor filename   
			= 							accWorld (fileExists filename)									// ??? does not seems to return False if the file does not exists
			>>- \exists ->			
			if (not exists)
				(						viewInformation () [] ("File " +++ filename +++ " does not exists")
				>>|						return ()
				)(						addToRecentFiles (readOnly,filename)
				>>|						readFromFile filename
				>>- \(_,content) ->		updAceLine content editorStore
				>>|						fileEditor` content 
				)
		where
			fileEditor` :: !String -> Task ()
			fileEditor` content  
			 	= 
			 	(				get recentFiles
				>>- \recent ->	get settings
				>>-	\curSet ->	(updateSharedInformation (dropDirectory filename, filename) [] editorStore @! ())
								||-
								updateSharedInformation (dropDirectory filename, filename) [UpdateUsing id (\_ nsc -> nsc) aceEditor] editorStore 
				>^*				(if readOnly 
								[]
								[ OnAction (Action "/Search/Find & Replace") (always (findAndReplace editorStore <<@ InWindow))
								, OnAction (Action "/Search/Find Selection") (always (findSelection editorStore <<@ InWindow))
								, OnAction (Action "/Edit/Shift Right") (always (shift True editorStore))
								, OnAction (Action "/Edit/Shift Left")  (always (shift False editorStore))
								])
				>>*				(if readOnly 
								[]
								[ OnAction  ActionSave    					 (hasValue (\rc -> saveFile   filename (fromAce rc) >>| fileEditor filename))
								, OnAction  ActionSaveAs					 (hasValue (\rc -> saveFileAs filename (fromAce rc) >>- fileEditor))
								, OnAction (Action "/File/Restore")			 (always (fileEditor filename))
								])
								++
								case (takeExtension filename,dropDirectory (dropExtension filename)) of 
									 ("icl",name) -> [ OnAction (Action ("/File/Open " +++ name +++ ".dcl")) (always (fileEditor (dropExtension filename +++ ".dcl")))]
									 ("dcl",name) -> [ OnAction (Action ("/File/Open " +++ name +++ ".icl")) (always (fileEditor (dropExtension filename +++ ".icl")))]
									 _			 -> []
								++ 
								[ OnAction (Action "/File/Open Recent Files") 	(ifCond (recent <> []) (openRecent True >>| fileEditor filename))
								, OnAction (Action "/File/Open File...") 		(always openFileDialog)
						 		, OnAction (Action "/Help/Cloogle")				(always  (jumpToCloogle >>| fileEditor filename))
								, OnAction (ActionClose)						(always	(return ()))
								]
				) 
				where
					saveFile :: !FilePath !String -> Task ()
					saveFile sname mcontent 
						= 							readFromFile sname
						>>- \(_,ncontent) -> 	
						if (ncontent <> content) 	// someone else (another editor on same file) may have changed the file content in the meantime
							(		viewInformation "Cannot save, file has been changed elsewhere, restore first" [] ""
							>>| 	return ()
							) 
							(		writeToFile sname mcontent @! ()
							)  
	
					saveFileAs :: FilePath String -> Task String
					saveFileAs saname mcontent 
						= 		updateInformation "Where to write to ?" [] saname
	
						>>*		[	OnAction ActionSave		(hasValue (	\name -> 	addToRecentFiles (False,name)
																		>>|			writeToFile name mcontent 
																		>>| 		return name))
							 	,	OnAction ActionCancel	(always (return saname))
							 	]
						>>= return 		// this is needed while I would expect it is not !!!
	
		openFileDialog :: Task ()
		openFileDialog = open <<@ ApplyLayout frameCompact
		where
			open
				=	mbCancel 
				(					updateInformation "Enter Filename" [] ".dcl"
				>>= \fileName -> 	findFileInProjectEnv fileName
				>>- \mbFound ->		case mbFound of
										Nothing 				-> viewInformation "Cannot find file " [] fileName >>| return ()
										Just (readOnly,path)	-> jumpToEditorPage True readOnly (path</>fileName)
				)
	
// ------------- find and replace

:: FindReplace = { find 		:: String
				 , replaceWith 	:: String
				 , wrapAround	:: Bool
				 , backward		:: Bool
				 }
derive class iTask FindReplace

findAndReplace :: !(Shared (!AceOptions,!AceState)) -> Task ()
findAndReplace editorStore
	= 	withShared {find = "", replaceWith = "", wrapAround = False, backward = False}
	(\findReplace ->
	(	updateSharedInformation (Title "Find & Replace") [] findReplace	// BUG ??? Buttons stay disabled for any value
	>^*	[ OnAction (Action "Find") 		 (ifValue (\fr -> True /*fr.find <> ""*/) 	        (\fr -> find 	fr.wrapAround fr.backward fr.find editorStore @! ()))  
		, OnAction (Action "Replace")    (ifValue (\fr -> True /* fr.replaceWith) <> ""*/)  (\fr -> replace fr.wrapAround fr.backward False fr.find fr.replaceWith editorStore))  
		, OnAction (Action "Replace All")(ifValue (\fr -> True /* fr.replaceWith) <> ""*/)  (\fr -> replace fr.wrapAround fr.backward True fr.find fr.replaceWith editorStore))  
		] @! ()
	>>* [ OnAction (ActionCancel)		(always (return ()))
		]
	))

replace :: !Bool !Bool !Bool !String !String !(Shared (!AceOptions,!AceState)) -> Task ()
replace wrapAround backward all searchString replaceString editorStore
	=						get editorStore
	>>= \(options,state=:{selection}) -> 
					case selection of
						Nothing ->		find wrapAround backward searchString editorStore				// find next
						Just {start=(idx,begin),end=(_,end)} 											// already something selected
								->		if ((state.lines!!idx)%(begin,end) == searchString)				// and it happens to be the search string
											(return selection)											// replace this one
											(find wrapAround backward searchString editorStore)			// find next
	>>= \found -> 	
		case found of
			Nothing 	->	return ()
			Just range	->			replaceLine range backward replaceString editorStore
							>>|		if all (replace wrapAround backward all searchString replaceString editorStore)
									(return ())																	

replaceLine :: !AceRange !Bool !String !(Shared (!AceOptions,!AceState)) -> Task ()
replaceLine {start = (lineNo,begin),end = (_,end)} backward replaceString editorStore
	= upd (\(options,state) -> (options,{state 	& lines 	= updateAt lineNo (updLine (state.lines!!lineNo)) state.lines
												, cursor 	= (lineNo,if backward begin (begin + size replaceString + 1))
												, selection = Nothing
										})) editorStore
	@! ()
where
	updLine line = line%(0,begin-1) +++ replaceString +++ line%(end,size line - 1)
		
find :: !Bool !Bool !String !(Shared (!AceOptions,!AceState)) -> Task (Maybe (AceRange))
find wrapAround backward searchString editorStore
	=						 get editorStore
	>>= \(options,state) ->	 let sel = allFound state.lines state.cursor in
							 if (isEmpty sel)
							 	 	(return Nothing)
							 	 (let range = hd sel in
							 	 		upd (\(options,state) -> (options,	{state 	& selection	= (Just range)
							 									       				, cursor	= if backward range.start range.end
							 												})) editorStore 
							 		>>| return (Just range)
							 	 )
where
	allFound :: ![String] !(!Int,!Int) -> [AceRange]
	allFound lines (lineNo,colNo) 
		| not backward	= tillEnd 			++ if wrapAround fromBegin []
		| otherwise		= reverse fromBegin ++ if wrapAround (reverse tillEnd) []		
	where
		tillEnd 	= 	[{start = (idx,begin), end = (idx,begin+size searchString)} 
						\\ idx <- [lineNo .. length lines - 1]
						,  begin <- [if (idx==lineNo) colNo 0 .. size (lines!!idx)  - size searchString] 
						| (lines!!idx)%(begin,begin + size searchString - 1) == searchString
						]
		fromBegin	=	[{start = (idx,begin), end = (idx,begin+size searchString)} 
						\\ idx <- [0 .. lineNo]
						,  begin <- [0 .. (if (idx==lineNo) colNo (size (lines!!idx)))  - size searchString] 
						| (lines!!idx)%(begin,begin + size searchString - 1) == searchString
						]

findSelection :: !(Shared (!AceOptions,!AceState)) -> Task ()
findSelection editorStore 
	=  						get editorStore
	>>= \(options,state) ->	if (isNothing state.selection)
							(return ()) 
							(let {start=(lineNo,beginSel),end=(_,endSel)} = fromJust (state.selection) 
							 in	find True False ((state.lines!!lineNo)%(beginSel,endSel-1)) editorStore @! ())

shift :: !Bool !(Shared (!AceOptions,!AceState)) -> Task ()
shift right editorStore
	=						get editorStore
	>>= \(options,state) ->	
	if (isNothing state.selection)
							(return ()) 
							(let {start=(lineBegin,_),end=(lineEnd,_)} = fromJust (state.selection) in
							 upd (\(options,nstate) -> (options,	{nstate	& lines = take lineBegin nstate.lines  ++ 
							 													  shifted lineBegin lineEnd nstate.lines ++ 
							 													  drop (lineEnd+1) nstate.lines
																			, selection = state.selection // BUG, update again because selection gets reseted after substitution
							 									})) editorStore @! () 
							 ) 
where
	shifted :: !Int !Int ![String] -> [String]
	shifted begin end lines 
	| right 	= ["\t" +++ lines!!idx 															\\ idx <- [begin..end]]
	| otherwise = [let line = lines!!idx in if (line%(0,0) == "\t") (line%(1,(size line) - 1)) line \\ idx <- [begin..end]]




// ------------- jump to error in editor

errorWindow :: !String !(Shared (!AceOptions,!AceState)) -> Task ()
errorWindow nameOfFileInEditor editorStore
	=				get project
	>>- \myProj ->	accWorld (fileExists (myProj.projectPath </> myProj.projectName +++ ".exe"))
	>>- \isExe ->	enterChoiceWithShared  (Title ("Errors & Warnings")) [ChooseFromGrid id] errorStore
	>^*				[ OnAction (Action "/File/Open File") (ifValue (\err -> fileName err <> dropDirectory nameOfFileInEditor) (findAndOpenFile myProj))
					, OnAction (Action "Goto Error") 	  (ifValue (\err -> fileName err == dropDirectory nameOfFileInEditor) (showError editorStore))
					]
	>>*				[ OnAction (Action "/Project/Build")			(always  Build)
				 	, OnAction (Action "/Project/Run")				(ifCond  isExe RunProject)
					, OnAction (Action "/Project/Set Settings")		(always  jumpToSetSettings)
				 	]						
	where
		findAndOpenFile myProj errorMessage 
			= 				findFileInProjectEnv (fileName errorMessage)
			>>- \mbFound -> case mbFound of
								Nothing 				-> viewInformation "Cannot find file " [] (fileName errorMessage) @! ()
								Just (readOnly,path)	-> jumpToEditorPage readOnly False path

		fileName :: String -> String
		fileName errorMessage = {c \\ c <- (takeWhile ((<>) ',') (tl (dropWhile ((<>) '[' ) [c \\ c <-: errorMessage])))}

		RunProject  
			= 				get project 
			>>- \myProj ->	runExec (myProj.projectPath </> myProj.projectName +++ ".exe") 8080 @! ()

		Build :: Task ()
		Build 
			= 				get settings 
			>>- \curSet ->	get project
			>>- \myProj ->	compile (curSet.cpmDirectory </> cpmFile) myProj.projectPath myProj.projectName
			>>|				readLinesFromFile (curSet.cpmDirectory </> errorFile) 
			>>- \errors ->  set errors errorStore @! ()

// ------------- parsing error messages from compiler

:: Error =  { kind 		:: String 		// error, info, parse error
			, file		:: FileName		// file name
			, line	 	:: (Int,Int)	// position
			, def		:: String		// kind of definition
			, info		:: String		// error message
			}

showError :: !(Shared (!AceOptions,!AceState)) !String  -> Task ()
showError  editorStore errorMessage
	# error = toError errorMessage
	= // viewInformation "error " [] error
		upd (\(options,state) -> (options,{state & cursor = error.line})) editorStore @! ()

toError :: String -> Error
toError errorMessage
	= 	{ kind 	= toString kind	
		, file	= toString file
		, line	= (max (lno - 1) 0,max (cno - 1) 0)	// aceEditor column no start counting with 0
		, def	= toString def	
		, info	= toString info
		}
where
	cno			= toInt (toString cn)
	lno 		= toInt (toString ln)
	(ln,cn)  	= mySpan ((<>) ';') pos
	(pos,def)  	= mySpan ((<>) ',') r1
	(file,r1)  	= mySpan ((<>) ',') position
	(kind,position) = mySpan ((<>) '[') before
	(before,info) 	= mySpan ((<>) ']') all

	mySpan pred list 
		= case span pred list of 
			(f,[]) -> (f,[])
			(f,s)  -> (f,tl s)

	all :: [Char]
	all = [c \\ c <-: errorMessage]

// -------------

