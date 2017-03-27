implementation module shares

import iTasks

:: Settings = 	{ dirCpm 	:: FilePath
				, dirClean	:: FilePath
				, dirClean2 :: FilePath
				, dirIDEEnvs :: FilePath
				, dirCmd :: FilePath
				}
derive class iTask Settings

settings :: Shared Settings
settings = sharedStore "settings" 	{ dirCpm = "C:\\Users\\Martin\\Documents\\clean-bundle-itasks-windows-x86-latest\\clean-bundle-itasks\\cpm.exe"
									, dirClean = "C:\Users\Martin\Documents\clean-bundle-itasks-windows-x86-latest\clean-bundle-itasks"
									, dirClean2 = "C:\\Users\\Martin\\Documents\\clean-classic-itasks-windows-x86-20161223"
									, dirIDEEnvs = "C:\\Users\Martin\\Documents\\clean-bundle-itasks-windows-x86-latest\\clean-bundle-itasks\\Config\\IDEEnvs"
									, dirCmd = "C:\\WINDOWS\\WinSxS\\wow64_microsoft-windows-commandprompt_31bf3856ad364e35_10.0.10586.0_none_21e70967f9147e9b\\cmd.exe"
									}

errorstate :: Shared String									
errorstate = sharedStore "errors" ""

content :: Shared String
content = sharedStore "content" ""