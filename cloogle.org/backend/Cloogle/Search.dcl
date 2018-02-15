definition module Search

/**
 * Search functions for the Cloogle system
 */

from Cloogle import :: Request, :: Result
from CloogleDB import :: CloogleDB

/**
 * Cloogle setting: whether to include language builtins if the Request has
 * Nothing for the include_builtin field.
 */
DEFAULT_INCLUDE_BUILTINS :== True

/**
 * Cloogle setting: whether to include library core modules if the Request has
 * Nothing for the include_core field.
 */
DEFAULT_INCLUDE_CORE :== False

/**
 * Search for a request in the type database
 */
search :: !Request !CloogleDB -> [Result]
