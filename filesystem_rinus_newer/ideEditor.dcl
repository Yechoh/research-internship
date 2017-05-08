definition module ideEditor

import iTasks
from ideStores import :: FileName

editFile 	:: !Bool !FileName -> Task () 	// Create an editor for the given file, True if readonly

editorPageName :== "/editor"

editorPage 			:: PublishedTask						// opens an editor in a browser page 

jumpToEditorPage 	:: !Bool !Bool !FilePath -> Task ()		// jumps to such a page, True if newpage, True if readonly  


