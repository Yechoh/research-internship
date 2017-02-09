definition module CpmLogic2

//if the filepath is a project, compile it and give the results in the temp folder
compile :: String *World -> *World

//creates a project as well as an icl file
//mainmodule: a location for the icl file
createProject :: String *World -> *World

//compiles a project and gives the results in a temp folder
//cleanhome: the clean folder
//pwd: i forgot, but giving the clean folder works
//pn: project location
buildProject :: String String String *World -> *World