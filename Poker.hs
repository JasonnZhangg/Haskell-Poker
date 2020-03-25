-- Worked in a team of TWO
-- student Name: Jason Zhang	 student #: 500839581
-- student Name: Zhi Long Peng   student #: 500901658

module Poker where 
	import Data.List

	deal cards = do
	--Seperate the hands 
		let handOne = sort [snd x | x <- (zip [1..10] cards), odd(fst x)]
		let handTwo = sort [snd x | x <- (zip [1..10] cards), even(fst x)]

		let winner = calculatePoints handOne handTwo		
		let stringifyList = if (winner == 1) then 
								stringifyHand (sort (zip (modHand handOne False) (handOne))) []
							else 
								stringifyHand (sort (zip (modHand handTwo False) (handTwo))) []
		stringifyList

	stringifyHand [] stringList = stringList
	stringifyHand hand stringList = do
		let card = snd (head hand) 
		let num = show (reduceSingleCard card False)
		let suit = suitChecker card
		if suit == 1 then 
			stringifyHand (tail hand) (stringList ++ [num ++ "S"])
		else if suit == 2 then  
			stringifyHand (tail hand) (stringList ++ [num ++ "H"])
		else if suit == 3 then
			stringifyHand (tail hand) (stringList ++ [num ++ "D"])
		else
			stringifyHand (tail hand) (stringList ++ [num ++ "C"])

	--Compare hands 
	comparePoints handO handT = do	
		--If one of the hands first index is 11 that means the other hand has the higher poker hand
		if (handO !! 0 < handT !! 0) then 1
		else if (handT !! 0 < handO !! 0) then 2
		
		--Tie Breaking
		--Royal Flush tie break
			--Check suit. The hand with the lower suit value has a better suit and wins 
		else if (handO !! 0 == 1 && handT !! 0 == 1) then
			if (handO !! 1) < (handT !! 1) then 1
			else 2
			
		--Straight Flush tie break
			--Check highest value, if tie, check suit 
		else if (handO !! 0 == 2 && handT !! 0 == 2) then
			if (handO !! 1) > (handT !! 1) then 1
			else if (handO !! 1) < (handT !! 1) then 2
			else 
				if (handO !! 2) > (handT !! 2) then 2
				else 1
				
		--Four of a kind Tie break
			--the higher value four of a kind wins 
		else if (handO !! 0 == 3 && handT !! 0 == 3)then
			if(handO !! 1) < (handT !! 1) then 2 
			else 1 
				
		--Full House tie break
			--The higher triple wins, check suit
		else if (handO !! 0 == 4 && handT !! 0 == 4) then
			if(handO !! 1) < (handT !! 1) then 2 
			else 1
			
		--Flush tie break
			--The highest value card with the highest suit wins 
			--index 1 is highest card suit, index 2-7 is the hand 
				--compareHighestValue checks which hand has the higher unique card, if none check suit
		else if (handO !! 0 == 5 && handT !! 0 == 5) then
			if compareHighestValue (drop 2 handO) (drop 2 handT) True == 1 then 1
			else if compareHighestValue (drop 2 handO) (drop 2 handT) True == 2 then 2
			else
				if (handO !! 1) < (handT !! 1) then 1
				else 2

		--Straight tie break
			--Check highest value card then highest suit wins 
		else if (handO !! 0 == 6 && handT !! 0 == 6) then
			if compareHighestValue (drop 2 handO) (drop 2 handT) False == 1 then 1
			else if compareHighestValue (drop 2 handO) (drop 2 handT) False == 2 then 2
			else 
				if (handO !! 1) < (handT !! 1) then 1 
				else 2
			
		--Three of a Kind tie break 	
			--Highest value triple wins
		else if (handO !! 0 == 7 && handT !! 0 == 7) then
			if (handO !! 1) > (handT !! 1) then 1 
			else 2

		-- 2 pairs
		else if (handO !! 0 == 8 && handT !! 0 == 8) then
			if pairTieBreak (drop 1 (take 4 handO)) (drop 1 (take 4 handT)) /= (-1) then 
				pairTieBreak (drop 1 (take 4 handO)) (drop 1 (take 4 handT))
			else 
				if (handO !! 4) < (handT !! 4) then 1
				else 2
			
		-- pair
		else if (handO !! 0 == 9 && handT !! 0 == 9) then
			if pairTieBreak  (drop 2 (take 5 handO))  (drop 2 (take 5 handT)) /= (-1) then
				pairTieBreak (drop 2 (take 5 handO)) (drop 2 (take 5 handT))
			else 
				if (handO !! 5) < (handT !! 5) then 1
				else 2
		
		--High Card tie break
		--The highest value card with the highest suit wins 
			--index 1 is highest card suit, index 2-7 is the hand 
				--compareHighestValue checks which hand has the higher unique card, if none check suit
		else
			if compareHighestValue (drop 2 handO) (drop 2 handT) True == 1 then 1
			else if compareHighestValue (drop 2 handO) (drop 2 handT) True == 2 then 2
			else
				if (handO !! 1) < (handT !! 1) then 1 
				else 2
	
	pairTieBreak [] [] = (-1)
	pairTieBreak (one:oneLast) (two:twoLast) = do 
		if (one > two) then 1
		else if (two > one) then 2
		else pairTieBreak oneLast twoLast
	
	--Function to calculate points for each hand 
		--Checks the type of poker hand of handOne and handTwo
		--If any match, compare the two hands 
	calculatePoints handOne handTwo = do
		if (isRoyalFlush handOne !! 0 == 1) || (isRoyalFlush handTwo !! 0 == 1) then
			comparePoints (isRoyalFlush handOne) (isRoyalFlush handTwo)
		else if (isStraightFlush handOne !! 0 == 2) || (isStraightFlush handTwo !! 0 == 2) then	
			comparePoints (isStraightFlush handOne) (isStraightFlush handTwo)
		else if (isFourKind handOne !! 0 == 3) || (isFourKind handTwo !! 0 == 3) then	
			comparePoints (isFourKind handOne) (isFourKind handTwo)
		else if (isFullHouse handOne !! 0 == 4) || (isFullHouse handTwo !! 0 == 4) then
			comparePoints (isFullHouse handOne) (isFullHouse handTwo)
		else if (isFlush handOne !! 0 == 5) || (isFlush handTwo !! 0 == 5) then
			comparePoints (isFlush handOne) (isFlush handTwo)
		else if (isStraight handOne !! 0 == 6) || (isStraight handTwo !! 0 == 6) then
			comparePoints (isStraight handOne) (isStraight handTwo)
		else if (isThreeKind handOne !! 0 == 7) || (isThreeKind handTwo !! 0 == 7)then
			comparePoints (isThreeKind handOne) (isThreeKind handTwo)
		else if (isTwoPair handOne !! 0 == 8) || (isTwoPair handTwo !! 0 == 8) then
			comparePoints (isTwoPair handOne) (isTwoPair handTwo)
		else if (isPair handOne !! 0 == 9) || (isPair handTwo !! 0 == 9) then
			comparePoints (isPair handOne) (isPair handTwo)
		else comparePoints (isHighest handOne) (isHighest handTwo)

	--Checks if it has a royal flush
		--Checks the suit, then card values
		--Return [hand type, suit]
	isRoyalFlush lst = do
		let temp = sort (modHand lst True) 
		let suit = checkHandSuit lst
		if (suit /= -1) && temp == [10,11,12,13,14] then [1, suit]
		else [11]

	--Checks if hand is straight flush	
		--Checks everything is consecutive, then checks suit
		--Returns [hand type, highest card value, suit]
	isStraightFlush lst	= do
		let temp = sort (modHand lst False)
		let suit = checkHandSuit lst 
		let min = head temp
		let max = temp !! 4
		if (suit /= (-1)) && (min + 4 == max) then 
			[2, max, suit]
		else 
			[11]
	
	--Checks if hand is Four of a Kind
		--Checks a sorted hand if 1st and 3rd or 2nd and 5th cards are equal meaning there is 4 of the same card
		--returns[hand type, value of four of a kind]
	isFourKind lst = do
		let temp = sort (modHand lst True)
		if (temp !! 0) == (temp !! 3) || (temp !! 1) == (temp !! 4) then [3, temp !! 2]
		else [11]	
	
	--Checks if hand is Full House
		--Checks if there is a triple and double
		--Returns [hand type, value of triple]
	isFullHouse lst = do
		let temp = sort (modHand lst True)
		if (temp !! 0) == (temp !! 2) && (temp !! 3) == (temp !! 4) || (temp !! 0) == (temp !! 1) && (temp !! 2) == (temp !! 4) then 
			[4, temp !! 2]
		else 
			[11]
		
	--Checks if hand is Flush
		--Check suit
		--Return [hand type, suit, rest of hand]
	isFlush lst = do
		let suit = checkHandSuit lst
		if suit /= -1 then [5, suit] ++ lst
		else [11]
		
	--Checks if hand is Straight	
		--Check if hand is consecutive
		--Return [hand type, highest card suit, lst]
	isStraight lst = do
		let temp = sort (modHand lst False)
		if ((head temp) + 4 == temp !! 4) then [6, suitChecker (maximum lst)] ++ lst
		else [11]
	
	--Checks if hand is Three of a Kind
		--Check for triple
		--Return [hand type, triple value]
	isThreeKind lst = do	
		let temp = sort (modHand lst True)
		if (temp !! 0) == (temp !! 2) || (temp !! 1) == (temp !! 3) || (temp !! 2) == (temp !! 4) then [7, temp !! 2]
		else [11]
		
	isTwoPair hand = do	
		let firstPairValue = findPair hand (-1)
		let secondPairValue = findPair hand firstPairValue
		let maxPair = maximum [firstPairValue, secondPairValue]
		let highCard = filter (\x -> (reduceSingleCard x True) == maxPair) hand
		let nonPairCards = filter(\x -> (reduceSingleCard x True) /= firstPairValue && (reduceSingleCard x True) /= secondPairValue) hand
		if (length (nub (modHand hand False)) == 3) then
			[8, maxPair, minimum [firstPairValue, secondPairValue]] ++ (reverse (sort (modHand nonPairCards True))) ++  [suitChecker (maximum highCard)]
		else
			[11]
		
	--Check if hand is pair
	isPair hand = do	
		let pairValue = findPair hand (-1)
		let highCard = filter (\x -> (reduceSingleCard x True) == pairValue) hand
		let nonPairCards = filter(\x -> (reduceSingleCard x True) /= pairValue) hand
		if (length (nub (modHand hand True)) == 4) then	
			[9, pairValue] ++ (reverse (sort (modHand nonPairCards True))) ++ [suitChecker (maximum highCard)]
		else
			[11]

	--Hand is high card 	
		--Returns [hand type, suit of highest card, hand]
	isHighest lst = do
		let temp = maximum (modHand lst True)
		let highest = findHigh lst temp
		let suit = suitChecker highest
		10:suit:lst	
	
	findHigh hand max = do
		if ((reduceSingleCard (hand !! 0) True )== max) then (hand !! 0)
		else
			findHigh (tail hand) max
		
	--Find the highest unique value
		--return winning hand, if all cards equal return -1 
	compareHighestValue [] [] _ = -1 
	compareHighestValue handOne handTwo isHighAce = do
		let one = reverse (sort (modHand handOne isHighAce))
		let two = reverse (sort (modHand handTwo isHighAce))
		if (one !! 0 == two !! 0) then
			compareHighestValue (tail one) (tail two) isHighAce
		else 
			if (one !! 0 > two !! 0) then 1
			else 2
		
	findPair hand foundNum = do
		let temp = modHand hand True
		fpair temp foundNum
		
	fpair [] _ = -1
	fpair (element:last) foundNum = do
		if (element /= foundNum) && (elem element last) then	
			element
		else
			fpair last foundNum

	--Finds the highest card in the hand
	findHighestS lst isHighAce = do
		let temp = modHand lst isHighAce
		maximum temp
	
	--Checks if card is certain suit
	suitChecker x = do
		if (x >= 1 && x <= 13) then 4
		else if (x >= 14 && x <= 26) then 3
		else if (x >= 27 && x <= 39) then 2
		else 1
		
	checkHandSuit hand = do 
		let temp = sort hand 
		let min = head temp
		let max = temp !! 4
		if (min > 0 && max < 14) then 4
		else if (min > 13 && max < 27) then 3
		else if (min > 26 && max < 40) then 2
		else if (min > 39 && max < 53) then 1
		else (-1)

	--mod the whole hand	
	modHand [] _ = []
	modHand(x:xs) isHighAce = do
		if(mod x 13 == 0)then	
			13:modHand xs isHighAce
		else if (isHighAce) && (mod x 13 == 1) then
			14:modHand xs isHighAce 
		else	
			(mod x 13):modHand xs isHighAce
			
	--mod a single card
	reduceSingleCard card isHighAce = do	
		if (mod card 13) == 0 then	
			13
		else if (isHighAce) && (mod card 13) == 1 then
			14 
		else mod card 13