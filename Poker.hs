module Poker where 
	import Data.List
	
	deal cards = do
	--Seperate the hands 
		let handOne = handOnef cards
		let handTwo = handTwof cards
		
		let handOneP = calculatePoints handOne
		let handTwoP = calculatePoints handTwo
		
		comparePoints handOneP handTwoP
	
	
	--Compare points
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
		
	
	--Function to calculate points for each hand 
		--The lower the points, the better the hand 
	calculatePoints lst = do
		if isRoyalFlush lst then 
			if allSpades lst then [1,1,1]
			else if allHearts lst then [1,1,2]
			else if allDiamonds lst then [1,1,3]
			else if allClubs lst then [1,1,4]
			else [11,0,0] 
			
		--Checks for Straight Flush	
		else if isStraight lst then
			if allSpades lst then [2, findHighestS lst, 1]
			else if allHearts lst then [2, findHighestS lst, 2]
			else if allDiamonds lst then [2, findHighestS lst, 3]
			else if allClubs lst then [2, findHighestS lst, 4]
			else [11,0,0]
		
		--Checks for Straight
		else if isStraight lst then
			if isSpades (findHighestS lst) then [6, findHighestS lst, 1]
			else if isHearts (findHighestS lst) then [6, findHighestS lst, 2]
			else if isDiamonds (findHighestS lst) then [6, findHighestS lst, 3]
			else if isClubs (findHighestS lst) then [6, findHighestS lst, 4]
			else [11,0,0]
			
		--Checks for Highest Card
		else 
			if isSpades (findHighestS lst) then [10, findHighestS lst, 1]
			else if isHearts (findHighestS lst) then [10, findHighestS lst, 2]
			else if isDiamonds (findHighestS lst) then [10, findHighestS lst, 3]
			else if isClubs (findHighestS lst) then [10, findHighestS lst, 4]
			else [11,0,0] 
		
	
	--Checks if it has a royal flush
	isRoyalFlush lst = do
		let temp = map (`mod` 13) lst 
		if all (\x -> x == 1|| x == 10 || x == 11 || x == 12 || x == 0) temp then True
		else False
		
	--Checks if hand is straight	
	isStraight lst	= do
		let temp = sort (map (`mod` 13) lst)
		let zipped = zip [head temp..] temp
		if all(\x -> fst x == snd x) zipped then True
		else False
	
	--Finds the highest card in the hand
	findHighestS lst = do
		let temp = map (`mod` 13) lst 
		maximum temp
		
	--Checks if the hand has same suit
	allClubs lst = all (\x -> isClubs x) lst
	allDiamonds lst = all (\x -> isDiamonds x) lst
	allHearts lst = all(\x -> isHearts x) lst
	allSpades lst = all(\x -> isSpades x) lst
	
	--Checks if card is certain suit
	isClubs x = 
		if(x>0 && x<14) then True
		else False 	
	isDiamonds x = 
		if(13<x && x<27) then True
		else False 
	isHearts x = 
		if(26<x && x<40) then True
		else False 
	isSpades x = 
		if(39<x && x<53) then True
		else False 		
	
	--Hand one become every odd nth card while Hand two is every even nth card
	handOnef lst = [snd x | x <- (zip [1..10] lst), odd(fst x)]
	handTwof lst = [snd x | x <- (zip [1..10] lst), even(fst x)]