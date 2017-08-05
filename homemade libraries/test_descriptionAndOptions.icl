module test_descriptionAndOptions

import iTasks
import Text
import descriptionAndOptions

:: Cow = 
	{
		male :: Bool,
		brown :: Bool,
		name :: String
	}

derive class iTask Cow

cows :: [Cow]
cows = 
	[
		{name="Alice",male=False,brown=False},
		{name="Bob",male=True,brown=False},
		{name="Claire",male=False,brown=True},
		{name="Dirk",male=True,brown=True}
	]

cowstore :: Shared [Cow]
cowstore = sharedStore "cows" cows
	
cowToDescription :: Cow -> String
cowToDescription c =
	concat [c.Cow.name,", the ",(if c.Cow.brown " brown " ""),(if c.Cow.male "bull" "cow")]
	
cowToOptions :: Cow -> [(String,Task String)]
cowToOptions c
	| c.Cow.male = [("Fight "+++c.Cow.name,return "you lost."),("Stare Contest",return "you won.")]
	| otherwise = [("Milk "+++c.Cow.name, return (if c.Cow.brown "got chocolatemilk." "got milk."))]

Start world =
	startEngine
	(
		viewListWithDescriptionAndOptions
		(map (\a. a.Cow.name) cows)
		(map (cowToDescription) cows)
		(map (cowToOptions) cows)
	)
	world

/*Start world =
	startEngine
	(
		(viewSharedListWithDescriptionAndOptions
		cowstore
		(\a. a.Cow.name)
		cowToDescription
		cowToOptions >>- \a.viewInformation "" [] a) 
	)
	world*/
		
		