implementation module helpwindowErrors

import iTasks
import doubleEnterChoice
import directoryBrowsing
import extraTaskCombinators
import shares
import content
import qualified Data.Map as DM
import extraText

helpwindowErrors :: Task ()
helpwindowErrors
	=
	(chooseTaskBasedOnElementOfSharedList
		""
		errorStore
		(\a.diagnosedErrorToDescription (diagnose a))
		(\a.diagnosedErrorToSolutions (diagnose a))
		(\a b.diagnosedErrorToTask (diagnose a) b)
	 )

:: DiagnosedError :== (FileName, Int, ErrorDiagnosis)
:: ErrorDiagnosis = UndefinedVar String | Unknown String

diagnosedErrorToDescription :: DiagnosedError -> String
diagnosedErrorToDescription (_,_,UndefinedVar varname) = varname+++" undefined"
diagnosedErrorToDescription (_,_,Unknown err) = "unknown error "+++err

diagnosedErrorToSolutions :: DiagnosedError -> [String]
diagnosedErrorToSolutions (filename,i,diagnosis)= ["Go to error"] ++
	case diagnosis of
	UndefinedVar varname =
		["Create "+++varname]
	_ = []

diagnosedErrorToTask :: DiagnosedError String -> Task ()
diagnosedErrorToTask (filename,line,_) "Go to error" = solutionGoToError filename line
diagnosedErrorToTask (filename,line,UndefinedVar varname) _ = solutionCreateVar filename line varname
diagnosedErrorToTask (filename,line,_) actiona = viewInformation "" [] ("undefined action "+++ actiona) >>| return ()

solutionGoToError :: String Int -> Task ()
solutionGoToError filename line
	= filenameToFilepath filename >>- \mfp.
	case mfp of
		Nothing = return ()
		Just filepath =
			contentOf filepath >>- \content.
			saveFile (filepath) (foldr joinWithNewline "" content.lines) >>|-
			upd (\c. 'DM'.del (filepath) c) contents >>|-
			setContents filepath >>|-
			jumpToLine filepath line


getErrorInt :: String -> Int
getErrorInt err
	#(rest,err) = splitOnce "[" err
	#(errinfo,errmessage) = splitOnce "]" err
	#[filename,line,obj:rest] = split "," errinfo
	= (toInt line) - 1

diagnose :: String -> DiagnosedError
diagnose err
	#(rest,err) = splitOnce "[" err
	#(errinfo,errmessage) = splitOnce "]" err
	#[filename,line,obj:rest] = split "," errinfo
	#errwords = split " " (dropChars 2 errmessage)
	#line = (toInt line) - 1
	| length errwords == 2
		| indexOf "undefined" (errwords!!1) <> -1
			= (filename,line,UndefinedVar (errwords!!0))
		| otherwise = (filename,line,Unknown errmessage)
	| otherwise = (filename,line,Unknown errmessage)

showButtons :: String [(Action,Task ())] String (ReadOnlyShared (Maybe String)) (Task ()) -> Task ()
showButtons message l str smstr t = viewInformation "" [] message ||- watch smstr >>*
		[(OnValue (ifValue (\mstr. mstr <> (Just str)) \a.t)):(map (\(a,t). OnAction a (always t)) l)]

startOfFunction :: String Int -> Task Int
startOfFunction filepath i = contentOf filepath >>- \c. return (sof c.lines (i+1))
	where
	sof :: [String] Int -> Int
	sof l 0 = 0
	sof l i
		| (indexOf "::" (l!!i)) == -1 && (indexOf "Start" (l!!i)) == -1	=  sof l (i-1)
		| otherwise						= i

solutionCreateVar :: String Int String -> Task ()
solutionCreateVar filename i var = filenameToFilepath filename >>- (\a. case a of
	Nothing = viewInformation "" [] "cannot find file" >>| return ()
	Just filepath =
		startOfFunction filepath i >>- \funi.
		placeText filepath funi [var+++" :: ",""] >>|-
		jumpToLine filepath funi
	)
