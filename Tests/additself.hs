module AddSelf where
	import Data.List

	addSelf x y = do
		let temp = x + y
		temp
	isStraight lst	= do
		let temp = sort (map (`mod` 13) lst)
		let zipped = zip [head temp..] temp
		if all(\x -> fst x == snd x) zipped then True
		else False
		
	findHighest lst = do
		let temp = map (`mod` 13) lst 
		maximum temp + 3
		
	comparePoints handO handT = do	
		if head handO < head handT then "handO win"
		else if head handO > head handT then "handT win"
		else 
			if (handO !! 1) > (handT !! 1) then "handO win"
			else if (handO !! 1) < (handT !! 1) then "handT win"
			else 
				if (handO !! 2) > (handT !! 2) then "handO win"
				else if (handO !! 2) < (handT !! 2) then "handT win"
				else "tie"
				
	theNumber x = do
		addSelf (findHighest x) 5 
		