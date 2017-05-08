implementation module webPm

import iTasks

import PmProject

derive class iTask Project, ApplicationOptions, CodeGenOptions,ProjectDynamicInfo, LinkOptions, LinkMethod, Output
derive class iTask UndefModule, UndefSymbol, InfListItem, StaticLibInfo, ModInfo
derive class iTask ABCLinkInfo, ModEditOptions, CompilerOptions, EditWdOptions, ListTypes
derive class iTask OptionalWindowPosAndSize, EditOptions, WindowPos_and_Size, NewlineConvention, [!!]

projectFileEditor :: FilePath FilePath -> Task()
projectFileEditor projectPath applicationDir 
	= 						accWorld (accFiles (ReadProjectFile projectPath applicationDir))
	>>= \(prj,ok,msg) ->	viewInformation "Current Project File Options" [] (ok, prj) 
	>>*						[ OnAction ActionEdit 	(always (editProject prj))
							, OnAction ActionClose 	(always (return ()))
							]
where
	editProject :: Project -> Task ()
	editProject project
		=		updateInformation "Change Project Settings" [] project 
		>>*		[ OnAction (Action "Save") (hasValue (\npr -> 		accWorld (accFiles (SaveProjectFile projectPath npr applicationDir)) 
																>>| projectFileEditor projectPath applicationDir))
				, OnAction ActionCancel    (always   (projectFileEditor projectPath applicationDir))
			]

		