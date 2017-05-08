definition module ideConstants

import System.FilePath, StdString, StdList

cpmFile			 :== "cpm.exe"
cleanIDE		 :== "CleanIDE.exe"
errorFile		 :==  "Temp" </> "errors"

isCleanFile 	file	:== isMember (takeExtension file) ["icl", "dcl", "prj", "abc", "sapl","html"]
isCPM       	file 	:== file == cpmFile
isCleanIcl  	file 	:== takeExtension file == "icl"
isCleanIclDcl 	file 	:== isMember (takeExtension file) ["icl", "dcl"]
anyFile 		file	:== True

