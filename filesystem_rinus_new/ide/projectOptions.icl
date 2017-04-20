implementation module projectOptions

import iTasks

import PmProject

derive class iTask Project, CodeGenOptions, InfListItem, LinkOptions, ApplicationOptions, LinkMethod, Output, ModInfo
derive class iTask ABCLinkInfo, ModEditOptions, CompilerOptions, EditWdOptions, ListTypes, EditOptions, [!!]
derive class iTask StaticLibInfo, ProjectDynamicInfo, UndefModule, OptionalWindowPosAndSize, NewlineConvention, UndefSymbol, WindowPos_and_Size

projectOptionsEditor :: FilePath FilePath -> Task ()
projectOptionsEditor projectPath applicationPath
	= 							accWorld (accFiles (ReadProjectFile projectPath applicationPath))
	>>- \(project,ok,err) ->	showSettings project
	@! ()
where
	showSettings project		
		=	viewInformation ("Current Project Options: " +++ projectPath) [] project
		>>*	[ OnAction (Action "Change") (always (changeSettings project))
			, OnAction (Action "Ok")	 (always (return ()))
			]

	changeSettings project
		=	updateInformation ("Change Project Options: " +++ projectPath) [] project
		>>*	[ OnAction (Action "Change") (hasValue storeSettings)
			, OnAction (Action "Cancel") (always (showSettings project))
			]

	storeSettings project
		=			accWorld (accFiles (SaveProjectFile projectPath project applicationPath))
		>>- \ok ->	showSettings project
	

