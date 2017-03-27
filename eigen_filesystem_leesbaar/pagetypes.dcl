definition module pagetypes

import iTasks

:: ChooseFileRedirects :== String String -> (Action,Task ())
:: EditorRedirects :== ((Action,Task ()),(Action,(String String -> Task ()) ) )
:: AskImportPathsRedirects :== ((Action, (String String -> Task ()) ), (Action, (String String -> Task ()) ) )