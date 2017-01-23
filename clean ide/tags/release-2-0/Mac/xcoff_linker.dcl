definition module xcoff_linker;

from StdFile import Files;
from StdString import String;

import linker2;

mark_used_modules :: !Int !Int !*{#Bool} !{#Int} !*{#*Xcoff} -> (![String],!*{#Bool},!*{#*Xcoff});
link_xcoff_files :: ![String] ![String] !String !(!Int, !{#Char}) !Int !Int !Int !Int !Int !Int !Int !Bool !*Files -> ((!Bool,![String]),!*Files);

