#!/bin/sh
CLM=clm

(cd backendC/CleanCompilerSources; make -f Makefile.linux64)
(cd main/Unix; make -f Makefile all);
$CLM -gcm -h 80M -s 4m -nt -nw -ci -nr -I backend -I frontend -I main -I main/Unix \
	-IL ArgEnv \
	-l main/Unix/Clean\ System\ Files/cDirectory.o \
	-l main/Unix/Clean\ System\ Files/set_return_code_c.o \
	-l main/Unix/Clean\ System\ Files/ipc_c.o \
	-l backendC/CleanCompilerSources/backend.a \
	cocl -o cocl
