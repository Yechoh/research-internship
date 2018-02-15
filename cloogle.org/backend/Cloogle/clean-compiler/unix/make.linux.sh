#!/bin/sh
CLM=clm

(cd backendC/CleanCompilerSources; make)
(cd ../libraries/ArgEnvUnix; make ArgEnvC.o)
(cd main/Unix; make -f Makefile all);
$CLM -h 40M -nt -nw -ci -ns -nr -I backend -I frontend -I main -I main/Unix \
	-IL ArgEnv \
	-l main/Unix/cDirectory.o \
	-l main/Unix/set_return_code_c.o \
	-l main/Unix/ipc.o \
	-l backendC/CleanCompilerSources/backend.a \
	cocl -o cocl
