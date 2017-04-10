implementation module tempfiles

import Text

totemp :: String -> String
totemp name = "temporal__"+++name

fromtemp :: String -> String
fromtemp name = replaceSubString "temporal__" "" name

totemptxt :: [String] String -> [String]
totemptxt [hd:rest] name = ["module "+++name+++" \\"+++hd:rest]

fromtemptxt :: [String] String -> [String]
fromtemptxt [hd:rest] name = [(replaceSubString ("module "+++name+++" \\") "" hd):rest]