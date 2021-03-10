--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Mon Jan 11 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Description: Verification3
--------------------------------------------------
module Verification3 where

import Constants

-- Input  [(outputY[0], outputY[1])] ->
--        [recoveryCounter]
-- Output [states]
verification3 :: (Fractional a, Ord a) => [(a,a)] -> [Int] -> ([Int],[Int])
verification3 [] _ = ([],[])
verification3 _ [] = ([],[])
verification3 ((outputY0, outputY1):remainingSignal) (counter:remainingCounters) =
    if outputY0 * outputY1 > 0 then
        if counter > 0 then
            (fromInteger recoveryState : nextStates, (counter - 1) : nextCounters)
        else
            (fromInteger correctState : nextStates, 0 : nextCounters)
    else
        (fromInteger erroneousState : nextStates, fromInteger cyclesPerSecond : nextCounters)

    where {
        (nextStates, nextCounters) = verification3 remainingSignal remainingCounters
    }