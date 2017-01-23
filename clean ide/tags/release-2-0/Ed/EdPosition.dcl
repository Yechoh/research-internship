definition module EdPosition

from StdClass import class <, class ==

:: Position
	= { col		:: ColumnNr
	  , row		:: LineNr
	  }

:: ColumnNr	 :== Int
:: LineNr	 :== Int

instance < Position
instance == Position
