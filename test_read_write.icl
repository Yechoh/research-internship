module test_read_write

import iTasks
import StdFile, System.File
import Data.Error
import qualified Data.Map as DM
import iTasks.UI.Layout, iTasks.UI.Definition, iTasks.UI.Editor.Builtin
import iTasks.API.Extensions.Editors.Ace

path :: String
path = "C:/Users/Martin/Documents/clean-classic-itasks-windows-x86-20161223/Libraries/iTasks-SDK/research internship/text.txt"

Start :: *World -> *World
Start world = startEngine (readandwrite) world

readandwrite :: Task ()
readandwrite = readFromFile path >>- \content. (viewInformation ""  [] content) >>= \c. writeToFile path c >>| return ()

//[UpdateUsing id (\_ nsc -> nsc) aceTextArea] geen extra enters
//[ViewUsing id aceTextArea] geen extra enters
//[ViewUsing id (textArea 'DM'.newMap)] geen extra enters
//[] wel extra enters

readFromFile :: String -> Task String
readFromFile path = worldIO (read path) 
where 
	read path world
	# (ok,file,world)			= fopen path FReadData world
	| not ok					= (Error ("Cannot find file: " +++ path), world) 
	# (res,file)				= readAll file
	# (ok,world)				= fclose file world
	| not ok					= (Error ("Cannot close file: " +++ path), world)
    = case res of
        Error e                 = (Error ("Cannot read File:" +++ path), world)
        Ok content              = (Ok content, world)

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