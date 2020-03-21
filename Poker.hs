module Poker where 
    import Data.List
    
    deal deck = do
        let handOne = [snd card | card <- (zip [1..10] deck), odd(fst card)]
        let handTwo = [snd card | card <- (zip [1..10] deck), even(fst card)]
        handTwo
    
    -- mod the whole hand
    modHand [] = []
    modHand (x:xs) = do
        if (mod x 13 == 0) then
            13:modHand xs
        else
            (mod x 13):modHand xs
    
    -- mod a single card 
    reduceSingleCard card = do 
        if (mod card 13) == 0 then
            13
        else 
            mod card 13

    isPair hand = do 
        -- find pair
        let pairValue = findPair hand (-1)
        -- find the biggest value of the pair
        let highCard = filter (\x -> reduceSingleCard x == pairValue) hand
        -- list of all the non pair cards
        let nonPairCards = filter (\x -> reduceSingleCard x /= pairValue) hand
        if (pairValue /= (-1)) then
            -- [ score, biggest pair value, suit of the pair ], [ list of non pair cards ]
            [[9, pairValue, suitChecker (maximum highCard)], nonPairCards]
        else 
            [[11]]
    
    isTwoPair hand = do
        -- find the 2 pairs 
        let firstPairValue = findPair hand (-1)
        let secondPairValue = findPair hand firstPairValue
        -- find the biggest pair 
        let maxPair = maximum [firstPairValue, secondPairValue]
        -- get the highest suit card of the biggest pair
        let highCard = filter (\x -> reduceSingleCard x == maxPair) hand
        -- get all of the non pair cards
        let nonPairCards = filter (\x -> reduceSingleCard x /= maxPair) hand
        if (firstPairValue /= (-1) && secondPairValue /= (-1)) then
            -- [ score, biggest pair, smallest pair, suit of the biggest pair ], [ list of non pair cards ]
            [[8, maxPair, minimum [firstPairValue, secondPairValue], suitChecker (maximum highCard)], nonPairCards]
        else 
            [[11]]

    findPair hand foundNum = do 
        let temp = modHand hand
        fpair temp foundNum

    fpair [] _ = -1
    fpair (element:last) foundNum = do
        if (element /= foundNum) && (elem element last) then
            element
        else 
            fpair last foundNum

    suitChecker x = do
        if (x > 0 && x < 14) then 
            4
        else if (x > 13 && x < 27) then
            3
        else if (x > 26 && x < 40) then
            2
        else
            1