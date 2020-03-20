module Poker where 
	deal cards = do
		let handOne = handOnef cards
		let handTwo = handTwof cards
		handTwo
		
	handOnef lst = [snd x | x <- (zip [1..10] lst), odd(fst x)]
	handTwof lst = [snd x | x <- (zip [1..10] lst), even(fst x)]