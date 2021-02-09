--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Thu Jan 14 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Description: Verification 8
--------------------------------------------------
module Verification8 where

import Constants
cycleLimit = toInteger (verification8Limit * (500 `div` fromIntegral tc))

-- Input  [(outputAB[0], outputAB[1])] ->
--        [counter]
-- Output ([states],[counters])
verification8 :: [(Int,Int)] -> [Int] -> ([Int],[Int])
verification8 [] _ = ([],[])
verification8 _ [] = ([],[])
verification8 ((outputAB0,outputAB1):remainingSignal) (counter:previousCounters) = do
    let nextOutput = verification8 remainingSignal previousCounters
    if abs (outputAB0 - outputAB1) > (fromInteger constantO) then
        (fromInteger erroneousState : fst nextOutput, fromInteger cycleLimit : snd nextOutput)
    else if counter > 0 then
        (fromInteger recoveryState : fst nextOutput, (counter - 1) : snd nextOutput)
    else
        (fromInteger correctState : fst nextOutput, 0 : snd nextOutput)
