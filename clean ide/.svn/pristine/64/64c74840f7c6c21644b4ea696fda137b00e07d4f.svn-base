implementation module ArgEnv

import code from "ArgEnvC.obj"				// DvA

/*2.0
import StdEnv
0.2*/
//1.3
import StdArray, StdBool, StdString, StdEnum
//3.1

:: CString :== Int
NULL :== 0

:: EnvironmentVariable
    =	EnvironmentVariableUndefined
    |   EnvironmentVariable !.{#Char}


getEnvSize :: !{#Char} -> Int
getEnvSize _
	= code {
		.inline getEnv
			ccall ArgEnvGetEnvironmentVariableSizeC "S-I"
		.end
	}

copyEnv :: !Int !{#Char} -> {#.Char}
copyEnv _ _
	= code {
		.inline getEnv
			| Clean 1.1: use create_array
			| pushC	'?'
			| push_b	1
			| update_b	1 2
			| update_b	0 1
			| pop_b	1
			| create_array	CHAR 0 1

			| Clean 1.2 and later: use create_array_
			create_array_	CHAR 0 1

			push_a	1
			push_a	1
			ccall	ArgEnvGetEnvironmentVariableCharsC "SS-I"
			pop_b	1
			update_a	0 1
			pop_a	1
		.end
	}

getEnvironmentVariable :: !{#Char} -> *EnvironmentVariable
getEnvironmentVariable name
	| size == 0
		=	EnvironmentVariableUndefined
	| otherwise
		=	EnvironmentVariable (copyEnv size name`)
	where
		size
			=	getEnvSize name`
		name`
			=	name +++ "\0"

copy :: !Int !CString -> {#.Char}
copy length cString
	= code {
		.inline copy
			| Clean 1.1: use create_array
			| pushC	'\000'
			| push_b	1
			| update_b	1 2
			| update_b	0 1
			| pop_b	1
			| create_array	CHAR 0 1

			| Clean 1.2 and later: use create_array_
			create_array_	CHAR 0 1

			push_a	0
			ccall	ArgEnvCopyCStringToCleanStringC "IS-I"
			pop_b	1
		.end
	}

getCommandLineCount :: Int
getCommandLineCount 
	= code {
		.inline getCommandLineCount
			ccall ArgEnvGetCommandLineCountC "-I"
		.end
	}

getCommandLineArgument :: !Int -> (!Int, !Int)
getCommandLineArgument _
	= code {
		.inline getCommandLineArgument
			ccall ArgEnvGetCommandLineArgumentC "I-II"
		.end
	}

getArg :: !Int -> {#.Char}
getArg i
	=	copy size cString
	where
		(size, cString)
			=	getCommandLineArgument i

// Clean 1.1: getCommandLine :: {{#Char}}
getCommandLine :: {.{#Char}}
getCommandLine
	=	{getArg i \\ i <- [0 .. getCommandLineCount-1]}
