module linker;

import StdInt,StdFile;

import xcoff_linker;

file_names = [
//	"hd:Desktop Folder:cocl.lib"
//	"ANSI C.PPC.Lib",
//	"SIOUX.PPC.Lib",
//	"MWCRuntime.Lib"
//	"Ron’s Apps:Developing Tools:Metrowerks:Metrowerks CodeWarrior:Obsolete ANSI Libraries:Libraries:ANSI PPC:ANSI C.PPC.Lib",
//	"Ron’s Apps:Developing Tools:Metrowerks:Metrowerks CodeWarrior:Obsolete ANSI Libraries:Runtime:Runtime PPC:MWCRuntime.Lib"
	"Ron’s System:Desktop Folder:test link:cocl.lib"
];

library_file_names = [
/*	"Interface.lib",
	"Math.lib"
	"hd:Desktop Folder:Clean:StdEnv:Clean System Files:library0",
	"hd:Desktop Folder:Clean:StdEnv:Clean System Files:library1",
	"hd:Desktop Folder:Clean:StdEnv:Clean System Files:library2"
*/
];

Start world
	= (ok1,undefined_symbols);
{
	(ok1,undefined_symbols,files1)
		= link_xcoff_files file_names library_file_names "Ron’s System:Desktop Folder:CleanPrograms:a.xcoff"
				0x200000 16 0x80000 8 (80<<10) 0x200000 0 False files0;
	(files0,_) = openfiles world;
}
