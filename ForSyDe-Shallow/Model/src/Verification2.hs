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


-- Input  [(outputB[0][0], outputB[0][1], outputB[0][0], outputB[1][1])] ->
--        [(differenceSum0, differenceSum1)]
-- Output ([(state[0],state[1])],[(differenceSum[0], differenceSum[1])])
verification2 :: [(Int,Int,Int,Int)] -> [(Int,Int)] -> ([(Int,Int)],[(Int,Int)])
verification2 [] _ = ([],[])
verification2 _ [] = ([],[])
verification2 ((outputB00, outputB01, outputB10, outputB11):remainingOutputB) ((previousDifferenceSum0, previousDifferenceSum1):remainingDifferenceSums) =
    ((state0, state1) : fst nextOutput, (differenceSum0, differenceSum1) : snd nextOutput)
    where {
        state0 = fst computeIndex0;
        state1 = fst computeIndex1;
        differenceSum0 = snd computeIndex0;
        differenceSum1 = snd computeIndex1;
        computeIndex0 = verification2Compute (outputB00,outputB01) previousDifferenceSum0;
        computeIndex1 = verification2Compute (outputB10,outputB11) previousDifferenceSum1;

        nextOutput = verification2 remainingOutputB remainingDifferenceSums;
    }

-- Input  (outputB[x][0], outputB[x][1]) -> previousDifferenceSum -> 
--        (currentState, currentDifferenceSum)
-- Output (state[x], differenceSum[x])
verification2Compute :: (Int,Int) -> Int -> (Int,Int)
verification2Compute (outputB0, outputB1) previousDifferenceSum =
    if outputB0 /= outputB1 then do
        let differenceSum = previousDifferenceSum + abs (outputB0 - outputB1)
        if differenceSum > fromInteger constantJ then
            (fromInteger erroneousState, differenceSum)
        else
            (fromInteger correctState, differenceSum)
    else do
        let differenceSum = 0
        (fromInteger correctState, differenceSum)