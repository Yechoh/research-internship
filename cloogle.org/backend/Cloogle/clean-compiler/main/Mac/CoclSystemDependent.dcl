// this is for the PowerMac
definition module CoclSystemDependent

//1.3
from StdString import String
from StdFile import Files
//3.1
/*2.0
from StdFile import :: Files
0.2*/

PathSeparator
	:==	','
DirectorySeparator
	:==	':'

script_handler :: !{#Char} *Files -> (!Int,!*Files);

clean2_compile :: !Int -> Int;

clean2_compile_c_entry :: !Int -> Int;

ensureCleanSystemFilesExists :: !String !*Files -> (!Bool, !*Files)

set_compiler_id :: !Int -> Int;
