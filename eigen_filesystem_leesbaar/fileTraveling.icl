implementation module fileTraveling

import iTasks
import CloogleDB
import shares
import extraTaskCombinators
import directoryBrowsing
import extraText
import content

openLocation :: Location -> Task ()
openLocation (Location lib mod i j name) =
    get settings >>- \s.
    readLinesFromFile (filename s) >>- \mtext.
    maybe (viewInformation "" [] ("file not found: "+++(filename s)) >>| return ())
    (\text. setContent (filename s) ({zero & lines=text, cursor=(maybe 0 id j,0)})
    >>|- return ()) mtext
    where
        filename s = s.cleanHome </> "lib" </> lib </> (replaceSubString "." (""</>"") mod) +++ ".icl"
openLocation (Builtin name) = return ()
openLocation (Project path mod i j name) =
    readLinesFromFile (filename) >>- \mtext.
    maybe (viewInformation "" [] ("local file not found: "+++filename) >>| return ())
    (\text. setContent (filename) ({zero & lines=text, cursor=(maybe 0 id j,0)}) >>|- return ()) mtext
    where
        filename = path </> (replaceSubString "." (""</>"") mod) +++ ".icl"
