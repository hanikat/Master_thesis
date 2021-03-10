--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Mon Jan 11 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Description: Verification 4
--------------------------------------------------
module Verification4 where

import Constants

-- Input  [(outputP[0], outputP[1])] ->
--        [(counter[0], counter[1])]
-- Output ([(state[0],state[1])], [(counter[0],counter[1])])
verification4 :: (Fractional a, Ord a) => [(a,a)] -> [(Int,Int)] -> ([(Int,Int)],[(Int,Int)])
verification4 [] _ = ([],[])
verification4 _ [] = ([],[])
verification4 ((outputP0, outputP1):remainingSignal) ((previousCounter0, previousCounter1):remainingCounters) = 
    ((state0,state1) : nextStates, (counter0,counter1) : nextCounters)
    where {
        (state0,counter0) = verification4Helper outputP0 previousCounter0;
        (state1,counter1) = verification4Helper outputP1 previousCounter1;
        (nextStates,nextCounters) = verification4 remainingSignal remainingCounters;
    }


-- Input  OutputP[x] ->
--        counter[x]
-- Output (state[x], counter[x])
verification4Helper :: (Fractional a, Ord a) => a -> Int -> (Int,Int)
verification4Helper outputP counter =
    if abs outputP > fromIntegral constantK then
        (fromInteger erroneousState, fromInteger cyclesPerSecond)
    else
        if counter > 0 then
            (fromInteger recoveryState, counter - 1)
        else
            (fromInteger correctState, 0)