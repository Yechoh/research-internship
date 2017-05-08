definition module createAndRunExec

import iTasks

//buildProject cpmBin buildDir iclFileName

createProject :: String String String   -> Task ()

//compile project  cpmBin buildDir iclFileName

compile :: String String String   -> Task ()

// run execPath portNumber

runExec :: String Int   -> Task ()
					
					

