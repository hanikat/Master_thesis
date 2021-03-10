--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Mon Jan 11 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Description: Verification 2
--------------------------------------------------
module Verification2 where

import Constants


recoveryCycles = (1000 `div` tc)

-- Input  [(outputB[0][0], outputB[0][1], outputB[0][0], outputB[1][1])] ->
--        [(differenceSum0, differenceSum1)]
-- Output ([(state[0],state[1])],[(differenceSum[0], differenceSum[1])])
verification2 :: [(Int,Int,Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> ([(Int,Int)],[(Int,Int)], [(Int,Int)])
verification2 [] _ _ = ([],[],[])
verification2 _ [] _ = ([],[],[])
verification2 _ _ [] = ([],[],[])
verification2 ((outputB00, outputB01, outputB10, outputB11):remainingOutputB) ((previousDifferenceSum0, previousDifferenceSum1):remainingDifferenceSums) ((previousCounter0,previousCounter1):previousCounters) =
    ((state0, state1) : nextStates, (differenceSum0, differenceSum1) : nextDifferences, (counter0,counter1) : nextCounters)
    where {
        (state0, differenceSum0, counter0) = verification2Compute (outputB00,outputB01) previousDifferenceSum0 previousCounter0;
        (state1, differenceSum1, counter1) = verification2Compute (outputB10,outputB11) previousDifferenceSum1 previousCounter1;
        (nextStates, nextDifferences, nextCounters) = verification2 remainingOutputB remainingDifferenceSums previousCounters;
    }

-- Input  (outputB[x][0], outputB[x][1]) -> 
--         previousDifferenceSum -> 
--         counter
-- Output (state[x], differenceSum[x], counter[x])
verification2Compute :: (Int,Int) -> Int -> Int -> (Int,Int,Int)
verification2Compute (outputB0, outputB1) previousDifferenceSum counter =
    if outputB0 /= outputB1 then do
        let differenceSum = previousDifferenceSum + abs (outputB0 - outputB1)
        if differenceSum > fromInteger constantJ then
            (fromInteger erroneousState, differenceSum, fromInteger cyclesPerSecond)
        else
            if counter > 0 then
                (fromInteger recoveryState, differenceSum, counter - 1)
            else 
                (fromInteger correctState, differenceSum, 0)
    else do
        let differenceSum = 0
        if counter > 0 then
            (fromInteger recoveryState, differenceSum, counter - 1)
        else 
            (fromInteger correctState, differenceSum, 0)