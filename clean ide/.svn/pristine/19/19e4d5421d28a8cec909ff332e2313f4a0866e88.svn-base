definition module PmDriver

import StdPSt
from PmProject		import :: Project, :: Pathname
from IdeState		import :: General
from PmCleanSystem	import :: CompileOrCheckSyntax

:: SetMadeProjectFun :==
	(	Bool
	->	Bool
	->	Project
	->	(PSt General) -> PSt General)

:: CleanupCont :==
	!Pathname
	!Bool
	!Bool
	!*(PSt *General) -> *(PSt *General)

CompileProjectModule ::					// Compile or Syntax-check a single module
	!CompileOrCheckSyntax
	!Pathname
	!Project
	!SetMadeProjectFun
	!*(PSt General) -> *(PSt General)

GenAsmProjectModule ::					// Generate assembly for a single module
	!.Pathname
	!Project
	!SetMadeProjectFun
	!*(PSt General) -> *(PSt *General)

BringProjectUptoDate ::					// Bring complete project up-to-date
	!Bool								// force recompile...
	CleanupCont
	!*(PSt *General) -> *PSt *General
