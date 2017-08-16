implementation module fileTraveling

openLocation :: Location -> Task ()
openLocation (Location lib mod i j name) =
    get settings >>- \s.
    setContent (filename s) (readLinesFromFile (filename s))
    
    >>|- return ()
    where
        filename s = s.cleanHome </> lib </> mod +++ ".icl"
openLocation ()
