module Main where
    import Poker

    main = do
        putStrLn "1 ----------------------------------"
        deal [11, 20, 12, 44, 13, 14, 1, 12, 10, 19]
        putStrLn "2 ----------------------------------"
        deal [11, 13, 24, 12, 16, 9, 37, 8, 21 ,5]
        putStrLn "3 ----------------------------------"
        deal [22, 12, 34, 25, 33, 17, 45, 7, 5, 3]
        putStrLn "4 ----------------------------------"
        deal [1, 2 ,14, 15 ,16 ,47, 35, 32, 10, 21]
        putStrLn "5 ----------------------------------"
        deal [39, 13, 7, 33, 31, 5, 42, 16, 2, 28]
        putStrLn "6 ----------------------------------"
        deal [13, 26, 12, 25, 9, 22, 8, 21, 5, 18]
        putStrLn "7 ----------------------------------"
        deal [1, 4, 14, 17, 27, 30, 40, 43, 3, 2]
        putStrLn "8----------------------------------"
        deal [10, 4, 9, 17, 8, 30, 7, 43, 6, 2]
        putStrLn " 9----------------------------------"
        deal [1, 14, 27, 40, 3, 5, 47, 23, 44, 52]
        putStrLn "10 ----------------------------------"
        deal [7, 33, 20, 46, 28, 1, 37, 14, 52, 3]
        putStrLn "11 ----------------------------------"
        deal [5, 31, 18, 44, 6, 19, 51, 38, 14, 27]
        putStrLn "12 ----------------------------------"
        deal [8 ,34 ,21, 47, 1, 14, 39, 52, 3, 42]
        putStrLn "13 ----------------------------------"
        deal [1, 14, 27, 40, 16, 29, 3, 42, 52, 39]
        putStrLn ""
        putStrLn "---- FLUSH TEST INPUTS ----"
        deal [1,15,3,16,5,18,7,20,9,22] 
        putStrLn "----------------------------------"
        deal [1,14,3,16,5,18,7,20,9,22]
        putStrLn "----------------------------------"
        deal [1,14,3,16,5,18,7,20,9,23]
        putStrLn "----------------------------------"
        deal [1,14,4,16,5,18,7,20,9,22]
        putStrLn "----------------------------------"
        deal [2,14,4,16,5,18,7,20,9,22]
        putStrLn "----------------------------------"
        putStrLn ""
        putStrLn "----,FOUR-OF-KIND,TEST,INPUTS,----"
        deal [13,1,26,14,39,27,52,40,2,4]
        putStrLn "----------------------------------"
        deal [12,13,25,26,38,39,51,52,2,4]
        putStrLn "----------------------------------"
        deal [10,12,23,25,36,38,49,51,2,4] 
        putStrLn "----------------------------------"
        deal [7,2,20,15,33,28,46,41,6,4]
        putStrLn "----------------------------------"
        putStrLn ""
        putStrLn "---- FULL HOUSE / THREE-OF-KIND TEST INPUTS ----"
        deal [1,13,14,26,27,39,2,11,15,24] 
        putStrLn "----------------------------------"
        deal [3,13,16,26,29,39,2,11,15,24] 
        putStrLn "----------------------------------"
        deal [3,1,16,27,29,40,2,11,17,25] 
        putStrLn "----------------------------------"
        deal [1,13,14,26,27,39,28,6,42,7] 
        putStrLn "----------------------------------"
        deal [2,13,15,26,28,39,36,6,42,7] 
        putStrLn "----------------------------------"
        putStrLn ""
        putStrLn "----,HIGH,CARD,----"
        deal [40,2,17,42,23,47,20,10,50,52] 
        putStrLn "----------------------------------"
        deal [2,1,16,43,31,47,26,11,22,5] 
        putStrLn "----------------------------------"
        deal [1,40,52,26,16,29,19,32,49,10] 
        putStrLn "----------------------------------"
        deal [2,15,52,26,16,29,19,32,49,10] 
        putStrLn "----------------------------------"
        deal [7,20,13,26,16,29,19,32,49,10] 
        putStrLn "----------------------------------"
        deal [1,40,52,11,16,29,19,32,49,10] 
        putStrLn "----------------------------------"
        deal [1,40,52,26,4,29,19,32,49,10] 
        putStrLn "----------------------------------"
        putStrLn ""
        putStrLn "---- PAIR TEST INPUTS ----"
        deal [1,14,27,40,3,5,47,23,44,52] 
        putStrLn "----------------------------------"
        deal [1,40,2,15,28,41,13,26,34,33] 
        putStrLn "----------------------------------"
        deal [2,15,28,41,14,26,3,11,4,25] 
        putStrLn "----------------------------------"
        deal [2,15,28,41,13,26,3,11,4,25]
        putStrLn "----------------------------------"
        deal [2,1,15,14,4,5,6,2,46,47]
        putStrLn "----------------------------------"
        deal [1,14,27,40,3,16,4,17,12,25] 
        putStrLn "----------------------------------"
        putStrLn ""
        putStrLn "----2 PAIR TEST INPUTS ----"
        deal [1,3,2,16,14,13,15,26,10,37]
        putStrLn "----------------------------------"
        deal [1,27,14,40,2,13,15,26,11,5] 
        putStrLn "----------------------------------"
        deal [2,28,15,41,4,30,17,43,1,13] 
        putStrLn "----------------------------------"
        deal [2,1,15,40,5,11,18,24,9,13] 
        putStrLn "----------------------------------"
        deal [2,15,28,41,4,17,30,43,12,25] 
        putStrLn "----------------------------------"
        putStrLn ""
        putStrLn "---- STRAIGHTS TEST INPUTS ----"
        deal [1,40,52,39,38,25,24,11,10,23] 
        putStrLn "----------------------------------"
        deal [1,9,52,10,38,11,24,12,10,39] 
        putStrLn "----------------------------------"
        deal [52,49,38,22,24,8,10,7,9,6]
        putStrLn "----------------------------------"
        deal [52,39,38,25,24,37,10,49,9,22] 
        putStrLn "----------------------------------"
        putStrLn ""
        putStrLn "Other test cases"
        putStrLn "50----------------------------------"
        deal [24,40,25,49,26,50,14,51,23,52]
        putStrLn "51----------------------------------"
        deal [2,41,3,42,4,43,5,44,6,45]
        putStrLn "52----------------------------------"
        deal [1,14,3,16,5,17,7,20,9,22]
        putStrLn "53----------------------------------"
        deal [16, 10, 14, 7, 17, 9, 15, 11, 18, 8]
        putStrLn "54----------------------------------"
        deal [17, 11, 16, 10, 18, 8, 15, 9, 14, 7]
        putStrLn "55----------------------------------"
        deal [30, 11, 2, 26, 31, 12, 32, 39, 3, 13]
        putStrLn "56----------------------------------"
        deal [32, 11, 31, 26, 30, 12, 3, 39, 2, 13]
        putStrLn "57----------------------------------"
        deal [5, 26, 1, 12, 9, 11, 7, 39, 3, 13]
        putStrLn "58----------------------------------"
        deal [5, 26, 3, 11, 9, 39, 7, 12, 1, 13]