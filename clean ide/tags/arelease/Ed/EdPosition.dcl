/*
 * EdPosition.dcl: the Position type represents positions within a text (sigh)
 */

definition module EdPosition

from StdClass import <, ==

:: Position
	= { col		:: ColumnNr
	  , row		:: LineNr
	  }

:: ColumnNr	 :== Int
:: LineNr	 :== Int

instance < Position
instance == Position

