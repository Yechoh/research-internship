// this is for Windows
definition module CoclSystemDependent

//1.3
from StdString import String
from StdFile import Files
//3.1
/*2.0
from StdFile import ::Files
0.2*/

// RWS split
// from deltaIOSystem import DeviceSystem
// from deltaEventIO import InitialIO, IOState

PathSeparator
	:==	','
DirectorySeparator
	:== ':'

SystemDependentDevices :: [a]
SystemDependentInitialIO :: [a]

ensureCleanSystemFilesExists :: !String !*Files -> (!Bool, !*Files)
set_compiler_id :: Int -> Int

compiler_loop :: ([{#Char}] *st -> *(Bool, *st)) *st -> (!Bool, !*st)

