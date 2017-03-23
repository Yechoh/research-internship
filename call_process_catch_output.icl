module call_process_catch_output

import qualified System.Process as SP
import iTasks

//de locaties van Target, temperr en templog zijn relatief vanaf de locatie van deze file
//de locaties van cpm en cmd zijn absoluut

dirCpm = "C:\\Users\\Martin\\Documents\\clean-bundle-itasks-windows-x86-latest\\clean-bundle-itasks\\cpm.exe"
dirTarget = "eigen_filesystem\\Temp\\webIDE.prj"
//C:\Users\Martin\Documents\clean-bundle-itasks-windows-x86-latest\research-internship\filesystem_rinus\webIDE.prj
Start :: *World -> *World
Start world = startEngine
	(accWorld('SP'.callProcess "C:\\WINDOWS\\WinSxS\\wow64_microsoft-windows-commandprompt_31bf3856ad364e35_10.0.10586.0_none_21e70967f9147e9b\\cmd.exe" ["/c",dirCpm,dirTarget,"1>eigen_filesystem\\Temp\\templog.txt","2>eigen_filesystem\\Temp\\temperr.txt"] Nothing)>>= \x.viewInformation "" [] x)
	world