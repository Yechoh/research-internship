/*
 * EdKeyMapping.dcl: configuring the key mapping of the editor
 */

definition module EdKeyMapping

from StdFile		import Files
from StdPSt			import PSt, IOSt
from StdIOCommon	import KeyboardState, SpecialKey, KeyState, Modifiers
from StdMaybe		import Maybe
from StdOverloaded	import ==, toString, fromString
from EdActionType	import Action
from EdMonad		import EditMonad, EditState, StateM

:: KeyMapping

// The first two arguments are there to circumvent the
// restriction that cyclic dependencies of definition modules are
// not allowed. The type should be:
//   configureKeyMapping :: (PSt EditorState .p) -> (PSt EditorState .p)
// But then you have to import EdState which imports this module...

configureKeyMapping :: KeyMapping (KeyMapping (PSt *l) -> (PSt *l)) (PSt *l) -> (PSt *l)

macKeyMapping		:: KeyMapping
//pcKeyMapping		:: KeyMapping

findAction			:: !KeyboardState !KeyMapping -> Maybe Action


ReadKeyMapFile	:: !{#Char} !*Files -> ((!KeyMapping, !Bool, !{#Char}),!*Files)
SaveKeyMapFile	:: !{#Char} !KeyMapping !*Files -> (!Bool, !*Files);

// avoid warnings
:: KeyCode
instance == KeyCode
instance toString KeyCode
instance fromString KeyCode
instance fromString SpecialKey
