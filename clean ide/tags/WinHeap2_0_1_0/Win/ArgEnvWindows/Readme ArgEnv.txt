ArgEnv
Version  1.0.1
Ronny Wichers Schreur
ronny@cs.kun.nl

The ArgEnv package provides a Clean interface to the command line
arguments and the environment variables. 

This is the README for the Windows version. The Windows version has
been tested on Windows NT client 4.0 with Clean 1.2 & 1.3, but it
should work with all Windows versions with Clean 1.2 or better. To
use the package with Clean version 1.1 you will have to make a few
changes, which are documented in the source.

FILES

    README
        This file
    ArgEnv.dcl
        Definition of the interface
    ArgEnv.icl
        Implementation of the interface
    ArgEnvC.c
        Implementation of the C side of the interface
        (You do not have to compile this file, it is included
        in the modified cdebug.obj)
    cdebug.obj
        A modified version of cdebug.obj that includes the
        C side of the ArgEnv interface
    kernel_library
        A modified version of kernel_library that defines
        some extra symbols from kernel32.dll
    printenv.icl
        An example program that prints the value of an environment
        variable
    printenv.prj
        Project file for the example program

USAGE

To use the ArgEnv interface you have to link the object module that
is created from the module ArgEnvC.c.
There is no easy way to link additional object files with the older
CleanIDEs (version 1.2 and before). Therefore I have made a new
cdebug.obj that contains the object code from ArgEnvC.c. You should
replace cdebug.obj in the Clean System Files of the StdEnv with the
cdebug.obj that comes with this release.

If you use CleanIDE 1.3 or later you can just add this extra object
file to the Object Modules section in the Options->Link Options dialogue.

If you forget this, you will get the link error:
    Undefined symbols:
    _ArgEnvGetEnvironmentVariableSizeC
    _ArgEnvGetEnvironmentVariableCharsC
    _ArgEnvCopyCStringToCleanStringC
    _ArgEnvGetCommandLineCountC
    _ArgEnvGetCommandLineArgumentC

The ArgEnv interface also uses two additional symbols from kernel32.dll.
You should replace kernel_library in the Clean System Files of the StdEnv
with the kernel_library that comes with this release. If you forget this,
you will get the link error:
    Undefined symbols:
    __imp__GetEnvironmentVariableA@12

BUGS

There is no way to stop the Clean run-time system from interpreting
some of the command-line arguments.

If you start a Clean program from the command-line prompt, you still
have to "press any key" before the program quits.
