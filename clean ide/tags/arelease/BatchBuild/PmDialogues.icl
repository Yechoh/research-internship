implementation module PmDialogues

import StdPSt, StdPathname, UtilStrictLists

doPathsDialog :: !String !Pathname !Pathname (!List Pathname) ((!List Pathname) (PSt .l) -> (PSt .l)) (PSt .l) -> (PSt .l)
doPathsDialog _ _ _ _ f ps = ps
