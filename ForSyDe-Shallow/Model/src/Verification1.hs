--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Mon Jan 11 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Description: Verification1
--------------------------------------------------
module Verification1 where

import Constants

-- Limit in seconds which the conditions must be met in order to set erroneous state
timeLimit = 5
cycleLimit = timeLimit * (1000 `div` tc)

-- Input  [(outputC[0], outputC[1])] ->
--        [(outputW[0](t-1), outputC[1](t-1))] ->
--        [(previousState[0], previousState[1])] ->
--        [(counter[0], counter[1])]
-- Output ([(state[0],counter[0])],[(state[1],counter[1])])
verification1 :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> ([(Int,Int)],[(Int,Int)])
verification1 [] _ _ _ = ([],[])
verification1 _ [] _ _ = ([],[])
verification1 _ _ [] _ = ([],[])
verification1 _ _ _ [] = ([],[])
verification1 ((outputC0,outputC1):remainingOutputC) ((outputW0,outputW1):remainingOutputW) ((previousState0,previousState1):remainingPreviousStates) ((previousCounter0,previousCounter1):remainingCounters) = 
    ((state0,state1) : fst nextOutput, (counter0,counter1) : snd nextOutput)
    where {
        (state0, counter0) = verification1Compute (outputC0, outputW0, previousState0, previousCounter0);
        (state1, counter1) = verification1Compute (outputC1, outputW1, previousState1, previousCounter1);
        nextOutput = verification1 remainingOutputC remainingOutputW remainingPreviousStates remainingCounters;
    }


-- Input (outputC[x], outputW[x](t-1), previousState[x](t-1), counter[x](t-1))
-- Output (state, counter)
verification1Compute :: (Int,Int,Int,Int) -> (Int,Int)
verification1Compute (outputC, outputW, state, counter) = 
    -- Check if current signals are valid
    if (min outputC outputW) >= 0 || (max outputC outputW) <= 0 then
        if state == fromInteger correctState then
            (fromInteger correctState, 0)
        else if counter < fromInteger cycleLimit then 
                (fromInteger recoveryState, counter + 1)
            else
                (fromInteger correctState, 0) 
    -- Handle invalid signal
    else
        (fromInteger erroneousState, 0)






