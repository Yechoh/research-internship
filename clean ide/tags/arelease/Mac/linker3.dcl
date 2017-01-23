definition module linker3;

from StdFile import Files;
from StdString import String;
import linker2;

:: *Sections = Sections !*String !*String !Sections | EndSections;

write_output_file :: !Bool .{#Char} .Int !.Int !.Int !.LibraryList !.Int !.Int .Bool !*Sections !.Int !{#.Bool} !{#.Int} !*{#*Xcoff} *Files -> (!Bool,Int,Int,*Files);
mark_modules :: !Int !Int !*[*Xcoff] !Int !Int !Int !LibraryList -> (![String],!Int,!{#Bool},!{#Int},!*{#*Xcoff});
