
#include <stdio.h>
#include <windows.h>
#include "Clean.h"
#include "thread_message.h"

int get_message_number (void)
{
	return RegisterWindowMessage ("CleanCompiler");
}

int get_current_thread_id (void)
{
	return GetCurrentThreadId();
}

int start_compiler_process (CleanString compiler_path,CleanString compiler_directory,CleanString command,
							int *compiler_thread_id_p,int *compiler_thread_handle_p,int *compiler_process_handle_p)
{
	PSTR application_name,command_line,env,dir;
	STARTUPINFO si;
	PROCESS_INFORMATION pi;
	int r;

	application_name=CleanStringCharacters (compiler_path);
	dir=CleanStringCharacters (compiler_directory);
	command_line=CleanStringCharacters (command);
	env=NULL;
	
	si.cb = sizeof (STARTUPINFO);
	si.lpReserved = NULL;
	si.lpReserved2 = NULL;
	si.cbReserved2 = 0;
	si.lpDesktop = NULL;
	si.lpTitle = NULL;
	si.dwFlags = 0;

	r=CreateProcess (application_name,command_line,NULL,NULL,TRUE,DETACHED_PROCESS/*0*/,env,dir,&si,&pi);

	if (r!=0){
		*compiler_thread_id_p=pi.dwThreadId;
		*compiler_thread_handle_p=pi.hThread;
		*compiler_process_handle_p=pi.hProcess;
	} else {
		*compiler_thread_id_p=0;
		*compiler_thread_handle_p=0;
		*compiler_process_handle_p=0;
	}
	
	return r;
}

int get_integers_from_message (int wm_number,int *i1_p,int *i2_p)
{
	MSG message;
	int r;

	r=GetMessage (&message,NULL,wm_number,wm_number);

	if (r!=0){
		*i1_p=message.wParam;
		*i2_p=message.lParam;
	} else {
		*i1_p=0;
		*i2_p=0;
	}
	
	return r;
}

int get_integers_from_thread_message (int wm_number,int thread_handle,int *i1_p,int *i2_p)
{
	MSG message;
	int r;

	r=PeekMessage (&message,NULL,wm_number,wm_number,PM_NOREMOVE);
	while (r==0){
		r=MsgWaitForMultipleObjects (1,&thread_handle,0,INFINITE,QS_POSTMESSAGE);

		if (r==-1 || r==WAIT_OBJECT_0 || r==WAIT_ABANDONED_0){
			*i1_p=0;
			*i2_p=0;
			return 0;
		}

		r=PeekMessage (&message,NULL,wm_number,wm_number,PM_NOREMOVE);
	}
	
	r=GetMessage (&message,NULL,wm_number,wm_number);

	if (r!=0){
		*i1_p=message.wParam;
		*i2_p=message.lParam;
	} else {
		*i1_p=0;
		*i2_p=0;
	}
	
	return r;
}

int get_string_from_file_map_and_delete_map (int file_map,CleanString s)
{
	int l,i;
	char *chars,*p;
	
	chars=CleanStringCharacters (s);
	l=CleanStringLength (s);
	
	p=MapViewOfFile ((HANDLE)file_map,FILE_MAP_ALL_ACCESS,0,0,l);
	if (p==NULL)
		return 0;

	for (i=0; i<l; ++i)
		chars[i]=p[i];
				
	UnmapViewOfFile (p);

	CloseHandle ((HANDLE)file_map);
	
	return 1;
}

int send_string_to_thread (int thread_id,int process_handle,int wm_number,CleanString s)
{
	HANDLE file_map,file_map2;
	char *chars,*p1;
	int r,l;
	/*
	int a;
	*/

	chars=CleanStringCharacters (s);
	l=CleanStringLength (s);
	
	if (l==0 || chars[l-1]!='\0')
		return 0;

	file_map=CreateFileMapping ((HANDLE)0xFFFFFFFF,NULL,PAGE_READWRITE,0,l,NULL);
	if (file_map==NULL)
		return 0;

	p1=MapViewOfFile (file_map,FILE_MAP_ALL_ACCESS,0,0,l);
	if (p1==NULL)
		return 0;
		
	{
		char *s_p,*d_p,c;
		
		s_p=chars;
		d_p=p1;
		do {
			c=*s_p++;
			*d_p++=c;
		} while (c!='\0');
	}
	
	UnmapViewOfFile (p1);

	r=DuplicateHandle (GetCurrentProcess(),file_map,(HANDLE)process_handle,&file_map2,0,0,DUPLICATE_CLOSE_SOURCE | DUPLICATE_SAME_ACCESS);

	if (r==0)
		return 0;

	do {
		/*
		r=PostThreadMessage (thread_id,wm_number,a,l);
		*/
		r=PostThreadMessage (thread_id,wm_number,l,(int)file_map2);
	} while (r==0);
	
	return r;
}

int send_integers_to_thread (int thread_id,int wm_number,int i1,int i2)
{
	int r;
		
	do {
		r=PostThreadMessage (thread_id,wm_number,i1,i2);
	} while (r==0);
	
	return r;
}
