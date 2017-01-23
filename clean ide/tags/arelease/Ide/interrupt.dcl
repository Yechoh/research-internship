definition module interrupt

// version B of interrupt routines
// todo: reintegrate with version A (UtilInterrupt)

import StdId, StdPSt

StartIntr :: !(!Id,Id) .a (.Bool -> .(.a -> .(*(PSt .b) -> *(.a,*(PSt .b))))) !*(PSt .b) -> *(PSt .b)
StopIntr :: !(.a,!Id) !*(PSt .b) -> *(PSt .b)
ContIntr :: !(.a,!Id) !*(PSt .b) -> *(PSt .b)
