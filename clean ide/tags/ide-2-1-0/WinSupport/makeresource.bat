rc -r -dWIN32 winIde.rc
link -out:winIde.rsrc dummy.obj winIde.res

rc -r -dWIN32 winHeap.rc
link -out:winHeap.rsrc dummy.obj winHeap.res

rc -r -dWIN32 winTime.rc
link -out:winTime.rsrc dummy.obj winTime.res
