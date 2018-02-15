definition module dcls

import iTasks
import shares

readDcl :: [String] -> [(Sharenum,String,Comment)]

updateDclStore :: Task (Map String [(Sharenum,String,Comment)])

dclOf :: String -> Task [(Sharenum,String,Comment)]
