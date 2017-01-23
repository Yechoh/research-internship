/*
 * EdFile.dcl: reading and writing text files
 */

definition module EdFile

from StdFile   import FileSystem, Files
//1.3
from StdString import String
//3.1
from EdText	   import Text
import StdError, StdMaybe
from UtilNewlinesFile import NewlineConvention

readText	:: !String !*env -> ((Error Text,NewlineConvention,Bool), !*env) | FileSystem env
writeText	:: !String !NewlineConvention !Text	!*env -> (Maybe String, !*env)	| FileSystem env

