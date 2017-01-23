ml64 /c /nologo dummy.asm
rc -r -dWIN64 winIde.rc
link /nologo /out:winIde.rsrc dummy.obj /entry:mainCRTStartup /subsystem:console winIde.res
rc -r -dWIN64 winTime.rc
link /nologo /out:winTime.rsrc dummy.obj /entry:mainCRTStartup /subsystem:console winTime.res
rc -r -dWIN64 winHeap.rc
link /nologo /out:winHeap.rsrc dummy.obj /entry:mainCRTStartup /subsystem:console winHeap.res
