definition module pagetypes

import iTasks

:: ChooseFileRedirects :== (Action,Task ())
:: EditorRedirects :== ((Action,Task ()),(Action,Task ()),(Action,Task ()) )
:: AskImportPathsRedirects :== ((Action, Task ()) , (Action, Task ()) ) 
:: CreateFileRedirects :== ((Action, Task ()) , (Action, Task ()) ) 