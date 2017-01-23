module test_systemprocess

//import iTasks
import System.Process

//onnodig
//sp :== 'System.Process'.callProcess

//werkt. opent sublime text
//Start w = callProcess "C:\\Program Files (x86)\\Sublime Text 2\\sublime_text.exe" [] Nothing w

//callProces task. Moet eerst taskengine geopend worden.
//Start w = callProcess "testje" [] "C:\Program Files (x86)\Sublime Text 2\sublime_text" [] Nothing

//ik weet niet wat ze met cpm en clm bedoelen. wss clean compiler
Start w = callProcess "clm" [] Nothing w

