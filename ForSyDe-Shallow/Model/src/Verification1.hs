--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Mon Jan 11 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Input-Dependencies: Rrequirement 2, Requirement 21
-- Output-Dependencies: N/A
-- Requirement: Verification 1
-- Description: Verification1
--------------------------------------------------
module Verification1 where

import Constants

-- Limit in seconds which the conditions must be met in order to set erroneous state
timeLimit = 5
cycleLimit = timeLimit * cyclesPerSecond
recoveryStates = (0,0)

-- Input  [(outputC[0], outputC[1])] ->
--        [(outputW[0](t-1), outputW[1](t-1))] ->
--        [(previousState[0], previousState[1])] ->
--        [(counter[0], counter[1])]
--        [(recoveryCounter[0], recoveryCounter[1])]
-- Output ([(state[0],state[1])],[(counter[0],counter[1])],[(recoveryCounter[0],recoveryCounter[1])])
verification1 :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> ([(Int,Int)],[(Int,Int)])
verification1 [] _ _ _ = ([],[])
verification1 _ [] _ _ = ([],[])
verification1 _ _ [] _ = ([],[])
verification1 _ _ _ [] = ([],[])
verification1 ((outputC0,outputC1):remainingOutputC) ((outputW0,outputW1):remainingOutputW) ((previousState0,previousState1):remainingPreviousStates) ((previousCounter0,previousCounter1):remainingCounters) = do
    let recoveryStates = (recoveryState0,recoveryState1)
    ((state0,state1) : nextStates, (counter0,counter1) : nextCounters)
    where {
        (state0, counter0, recoveryState0) = verification1Compute (outputC0, outputW0, previousState0, previousCounter0, fst recoveryStates);
        (state1, counter1, recoveryState1) = verification1Compute (outputC1, outputW1, previousState1, previousCounter1, snd recoveryStates);
        (nextStates, nextCounters) = verification1 remainingOutputC remainingOutputW remainingPreviousStates remainingCounters;
    }


-- Input (outputC[x], outputW[x](t-1), previousState[x](t-1), counter[x](t-1), recoveryCounter[x](t-1))
-- Output (state, counter, recoveryCounter)
verification1Compute :: (Int,Int,Int,Int,Int) -> (Int,Int,Int)
verification1Compute (outputC, outputW, state, counter, recoveryCounter) =
    -- Check if current signals are valid
    if outputC * outputW > 0 then
        if recoveryCounter > 0 then
            if counter > 0 then 
                (fromInteger recoveryState, counter - 1, recoveryCounter - 1)
            else
                (fromInteger recoveryState, 0, recoveryCounter - 1)
        else
            if counter > 0 then 
                (fromInteger correctState, counter - 1, 0)
            else
                (fromInteger correctState, 0, 0)
    else if counter + 1 < fromInteger cycleLimit then
        if recoveryCounter > 0 then
            (fromInteger recoveryState, counter + 1, recoveryCounter - 1)
        else
            (fromInteger correctState, counter + 1, 0)
    else
        (fromInteger erroneousState, counter, fromInteger cyclesPerSecond)