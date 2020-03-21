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
		else if (isStraightFlush handOne !! 0 == 2) || (isStraightFlush handTwo !! 0 == 2) then	
			comparePoints handOne handTwo
		else if (isFourKind handOne !! 0 == 1) || (isFourKind handTwo !! 0 == 1) then	
			comparePoints handOne handTwo
		else if (isFullHouse handOne !! 0 == 1) || (isFullHouse handTwo !! 0 == 1) then
			comparePoints handOne handTwo
		else if (isFlush handOne !! 0 == 1) || (isFlush handTwo !! 0 == 1) then
			comparePoints handOne handTwo	
		else if (isStraight handOne !! 0 == 1) || (isStraight handTwo !! 0 == 1) then
			comparePoints handOne handTwo
		else if (isThreeKind handOne !! 0 == 1) || (isThreeKind handTwo !! 0 == 1)then
			comparePoints handOne handTwo
		else if (isTwoPair handOne !! 0 == 1) || (isTwoPair handTwo !! 0 == 1) then
			comparePoints handOne handTwo
		else if (isPair handOne !! 0 == 1) || (isPair handTwo !! 0 == 1) then
			comparePoints handOne handTwo
		else comparePoints handOne handTwo
		


		
	
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
			if allSpades lst [2,findHighestS zipped,1]
			else if allHearts lst then [2,findHighestS zipped,2]
			else if allDiamonds then [2,findHighestS zipped,3]
			else if allClubs then[2,findHighestS zipped,4]
		else [11]
	
	--Checks if hand is Four of a Kind
	isFourKind lst = do
		let temp = sort (modHand lst)
		if (temp !! 0) == (temp !! 3) || (temp !! 1) == (temp !! 4)then [3, temp !! 2]
		else [11]
		
	--Checks if hand is Full House
	isFullHouse lst = do
		let temp = sort (modHand lst)
		if (temp !! 0) == (temp !! 2) && (temp !! 3) == (temp !! 4) then [4, temp !! 0]
		else if (temp !! 0) == (temp !! 1) && (temp !! 2) == (temp !! 4) then [4, temp !! 2]
	
	--Checks if hand is Flush
	isFlush lst = do
		if allSpades lst then [5,1]
		else if allHearts lst then [5,2]
		else if allDiamonds then [5,3]
		else if allClubs then[5,4]
		
	--Checks if hand is Straight	
	isStraight lst = do
		let temp = sort (modHand lst)
		let zipped = zip [head temp..] temp
		if all(\x -> fst x == snd x) zipped then
			[6, findHighestS zipped, suitChecker findHighestS zipped]
		else [11]
	
	--Checks if hand is Three of a Kind
	isThreeKind lst = do	
		let temp = sort (modHand lst)
		if (temp !! 0) == (temp !! 2) then [7, temp !! 0]
		else if (temp !! 1) == (temp !! 3) then [7, temp !! 1]
		else if (temp !! 2) == (temp !! 4) then [7, temp !! 2]
		else [11]
		
	isTwoPair lst = do	
		--find the 2 pairs 
		let firstPairValue = findPair lst (-1)
		let secondPairValue = findPair lst (-1)
		--find the biggest pair
		let maxPair = maximum [firstPairValue, secondPairValue]
		--get the highest suit card of the biggest pair
		let highCard = filter (\x -> reduceSingleCard x == maxPair) hand
		--get all the non pair cards
		let nonPairCards = filter(\x -> reduceSingleCard x/= maxPair &&(reduceSingleCard x) /= minimum [firstPairValue, secondPairValue]
		if (firstPairValue /= (-1) && secondPairValue /= (-1)) then
			--[score, biggest pair, smallest pair, suit of the biggest pair], [list of non pair cards]
			[[8, maxPair, minimum[firstPairValue,secondPairValue],suitChecker(maximum highCard)],nonPairCards]
		else
			[[11]]
		
	--Check if hand is pair
	isPair lst = do	
		--find pair	
		let pairValue = findPair lst (-1)
		--find the biggest value of the pair
		let highCard = filter (\x -> reduceSingleCard x == pairValue) hand
		--list of all non pair cards
		let nonPairCards = filter(\x -> reduceSingleCard x /= pairValue) hand
		if (parValue /= (-1)) then	
			--[ score, biggest pair value, suit of the pair], [list of non pair cards]
			[[9, pairValue, suitChecker(maximum highCard)], nonPairCards]
		else
			[[11]]
			
	isHighest lst = do
		[10, maximum lst]
	
	
	findPair hand foundNum = do
		let temp = modHand hand
		fpair temp foundNum
		
	fpair []_ = -1
	fpair (element:last) foundNum = do
		if(element /= foundNum) && (elem element last) then	
			element
		else
			fpair last foundNum

	
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