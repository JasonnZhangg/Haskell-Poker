module Poker where 
	import Data.List
	
	deal cards = do
	--Seperate the hands 
		let handOne = handOnef cards
		let handTwo = handTwof cards
		
		let winner = calculatePoints handOne handTwo


	--Compare points
	comparePoints handO handT = do	

		
	
	--Function to calculate points for each hand 
		--The lower the points, the better the hand 
	calculatePoints handOne handTwo = do
		if (isRoyalFlush handOne !! 0 == 1) || (isRoyalFlush handTwo !! 0 == 1)then
			comparePoints handOne handTwo
		else if (isStraightFlush handOne !! 0 == 1) || (isStraightFlush handTwo !! 0 == 1) then	
			comparePoints handOne handTwo
		--Checks for Straight Flush	
		else if (isStraight handOne !! 0 == 1) || (isStraight handTwo !! 0 == 1) then
			comparePoints handOne handTwo
		else[11]
		
		--Checks for Straight
		
	
	--Checks if it has a royal flush
	isRoyalFlush lst = do
		if allSpades lst then 
			let temp = modHand lst 	
			if all (\x -> x == 1|| x == 10 || x == 11 || x == 12 || x == 0) temp then [1,1]
		else if allHearts lst then
			let temp = modHand lst 	
			if all (\x -> x == 1|| x == 10 || x == 11 || x == 12 || x == 0) temp then [1,2]
		else if allDiamonds lst then
			let temp = modHand lst 	
			if all (\x -> x == 1|| x == 10 || x == 11 || x == 12 || x == 0) temp then [1,3]
		else if allClubs lst then
			let temp = modHand lst 	
			if all (\x -> x == 1|| x == 10 || x == 11 || x == 12 || x == 0) temp then [1,4]
		else [11]
		
	--Checks if hand is straight flush	
	isStraightFlush lst	= do
		let temp = sort (modHand lst)
		let zipped = zip [head temp..] temp
		if all(\x -> fst x == snd x) zipped then
			if allSpades lst then [2,1]
			else if allHearts lst then [2,2]
			else if allDiamonds then [2,3]
			else if allClubs then[2,4]
		else [11]
		
	isStraight lst = do
		let temp = sort (modHand lst)
		let zipped = zip [head temp..] temp
		if all(\x -> fst x == snd x) zipped then
		
	
	findPairNum lst = do 
		let temp = modHand lst
		length temp - length (nub temp)
	
	--Finds the highest card in the hand
	findHighestS lst = do
		let temp = modHand lst 
		maximum temp
		
	--Checks if the hand has same suit
	allClubs lst = all (\x -> (suitChecker x) == 4) lst
	allDiamonds lst = all (\x -> (suitChecker x) == 3) lst
	allHearts lst = all(\x -> (suitChecker x) == 2) lst
	allSpades lst = all(\x -> (suitChecker x) == 1) lst
	
	--Checks if card is certain suit
	suitChecker x = do
		if (x > 0 && x < 14) then 4
		else if (x > 13 && x < 27) then 3
		else if (x > 26 && x < 40) then 2
		else 1
		
	--mod the whole hand	
	modHand [] = []
	modHand(x:xs) = do
		if(mod x 13 == 0)then	
			13:modHand xs
		else	
			(mod x 13):modHand xs
			
	--mod a single card
	reduceSingleCard card = do	
		if (mod card 13) == 0 then	
			13
		else mod card 13
	
	--Hand one become every odd nth card while Hand two is every even nth card
	handOnef lst = [snd x | x <- (zip [1..10] lst), odd(fst x)]
	handTwof lst = [snd x | x <- (zip [1..10] lst), even(fst x)]