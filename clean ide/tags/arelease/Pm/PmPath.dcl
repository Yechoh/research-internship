definition module PmPath


import StdFile, StdOverloaded, StdString
import StdPathname
from UtilStrictLists	import List
from PmTypes			import Modulename, Processor

IsDefPathname :: !Pathname -> Bool;
IsImpPathname :: !Pathname -> Bool;
IsABCPathname :: !Pathname -> Bool;
IsPrjPathname :: !Pathname -> Bool;

MakeDefPathname				:: !String				-> Pathname;
MakeImpPathname				:: !String				-> Pathname;
MakeABCPathname				:: !String				-> Pathname;
MakeObjPathname				:: !Processor !String	-> Pathname;
MakeProjectPathname			:: !String				-> Pathname;
MakeExecPathname			:: !String				-> Pathname;
MakeSystemPathname			:: !Pathname			-> Pathname;
MakeABCSystemPathname		:: !Pathname			-> Pathname
MakeObjSystemPathname		:: !Processor !Pathname	-> Pathname
MakeAssemblySystemPathname	:: !Pathname			-> Pathname
//MakeABCSystemPathname		:: !Pathname !Files -> (!Pathname,!Files);
//MakeObjSystemPathname		:: !Processor !Pathname !Files -> (!Pathname,!Files);

GetModuleName :: !Pathname -> Modulename;

/* The name of the system directory */

SystemDir			:== "Clean System Files";

symPath		:: !Pathname !Pathname !Pathname		-> Pathname	// appPath prjPath fulPath -> symPath
fulPath		:: !Pathname !Pathname !Pathname		-> Pathname	// appPath prjPath symPath -> fulPath
symPaths	:: !Pathname !Pathname !(List Pathname)	-> List Pathname
fulPaths	:: !Pathname !Pathname !(List Pathname)	-> List Pathname

symAppPath	:: !Pathname !Pathname			-> Pathname
fulAppPath	:: !Pathname !Pathname			-> Pathname
symAppPaths	:: !Pathname !(List Pathname)	-> List Pathname
fulAppPaths	:: !Pathname !(List Pathname)	-> List Pathname
