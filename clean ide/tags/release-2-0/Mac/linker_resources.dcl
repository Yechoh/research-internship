definition module linker_resources;

from StdString import String;
from StdFile import Files;

read_application_options :: !{#Char} !*Files -> (!Bool,(!Int, !{#Char}), !Int, !Int, !Int, !Int, !Int, !Int, !Int, !Int, !*Files);
create_application_resource :: !{#Char} /* RWS ... */ !Bool /* ... RWS */ (!Int, !{#Char}) !Int !Int !Int !Int !Int !Int !Int !*Files -> (!Bool,!*Files);
