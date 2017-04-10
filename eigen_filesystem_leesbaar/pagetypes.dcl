definition module pagetypes

import iTasks

:: ChooseFileRedirects :== (Action,String -> String -> Task ())
:: EditorRedirects :== ((Action,Task ()),(Action,(String -> String -> Task ()) ) )
:: AskImportPathsRedirects :== ((Action, (String -> String -> Task ()) ), (Action, (String -> String -> Task ()) ) )