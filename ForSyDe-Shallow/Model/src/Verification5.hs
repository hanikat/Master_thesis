--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Tue Jan 12 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Input-Dependencies: Requirement 12
-- Output-Dependencies: Verification 6
-- Requirement: Verification 5
-- Description: Verification 5
--------------------------------------------------
module Verification5 where

import Constants

cycleLimit = toInteger (30 * cyclesPerSecond)

-- Helper functions
-- Input (outputP[0], outputP[1])
-- Output Bool
isNonZero :: (Fractional a, Ord a) => (a,a) -> Bool
isNonZero (outputP0, outputP1) =
    if abs outputP0 > 0 && abs outputP1 > 0 then
        True 
    else
        False

-- Input (outputP[0], outputP[1]) -> 
--       (outputP[0](t-1), outputP[1](t-1))
-- Output Bool
isWithinThreshold :: (Fractional a, Ord a) => (a,a) -> (a,a) -> Bool 
isWithinThreshold (outputP0,outputP1) (outputP0Old,outputP1Old) = 
    if outputP0 > outputP1 then
        if (abs outputP0Old) - (abs outputP1) > fromInteger constantL then
            True 
        else
            False
    else
        if (abs outputP1Old) - (abs outputP0) > fromInteger constantL then
            True 
        else
            False

-- Input outputP[x] -> [outputP[x](-t)]
-- Output Bool, Used to indicate if all values in the list [O10[x](-t)] are within range from the currentValue O10[x]
isConstantOverTime' :: (Fractional a, Ord a) => a -> [a] -> Bool
isConstantOverTime' _ [] = True
isConstantOverTime' currentValue previousValues = 
    if abs (currentValue - maximum previousValues) > fromInteger constantM || abs (currentValue - minimum previousValues) > fromInteger constantM then
        False 
    else 
        True

-- Input (outputP[0], outputP[1]) -> 
--       [(outputP[0](t-1),outputP[0](t-1))...(outputP[0](t-cycleLimit),outputP[0](t-cycleLimit))]
-- Output Bool, Used to indicate if all values are within range for the instance with the current highest value
isConstantOverTime :: (Fractional a, Ord a) => (a,a) -> [(a,a)] -> Bool 
isConstantOverTime (outputP0,outputP1) listOfPreviousValues = 
    if outputP0 > outputP1 then
        isConstantOverTime' outputP0 (map fst (take (fromInteger cycleLimit) listOfPreviousValues))
    else
        isConstantOverTime' outputP1 (map snd (take (fromInteger cycleLimit) listOfPreviousValues))


-- Input  [(outputP[0], outputP[1])] ->
--        [(outputP[0](t-1),outputP[0](t-1))...(outputP[0](t-cycleLimit),outputP[0](t-cycleLimit))]
-- Output ([state],[(outputP[0],outputP[0])...(outputP[0](t-cycleLimit+1),outputP[0](t-cycleLimit+1))],[counter],[recoveryCounter])
verification5 :: (Fractional a, Ord a) => [(a,a)] -> [(a,a)] -> [Int] -> [Int] -> ([Int],[(a,a)],[Int],[Int])
verification5 [] _ _ _ = ([],[],[],[])
verification5 _ [] _ _ = ([],[],[],[])
verification5 _ _ [] _ = ([],[],[],[])
verification5 _ _ _ [] = ([],[],[],[])
verification5 (outputP:remainingOutputP) outputPOld (currentCounter:remainingCounters) (currentRecoveryCounter:remainingRecoveryCounters)= do
    ((state : nextState), (outputP : nextOldOutputP), (counter : nextCounter), (recoveryCounter : nextRecoveryCounter))
    where {
        (state, oldOutputP,counter,recoveryCounter) = verification5Helper outputP  outputPOld currentCounter currentRecoveryCounter;
        (nextState, nextOldOutputP, nextCounter, nextRecoveryCounter) = verification5 remainingOutputP oldOutputP remainingCounters remainingRecoveryCounters;
    }
    

-- Input (outputP[0], outputP[1]) ->
--       [(outputP[0](-t), outputP[1](-t))] ->
--       [counter]
-- Output (state, [(outputP[0],outputP[0])...(outputP[0](t-cycleLimit+1),outputP[0](t-cycleLimit+1))],counter,recoveryCounter)
verification5Helper :: (Fractional a, Ord a) => (a, a) -> [(a, a)] -> Int -> Int -> (Int,[(a,a)],Int,Int)
verification5Helper (outputP0, outputP1) ((outputP0Old,outputP1Old):oldSignal) counter recoveryCounter = do
    let newOldSignal = ((outputP0,outputP1) : (outputP0Old,outputP1Old) : init oldSignal)
    if isNonZero (outputP0,outputP1) &&
       isWithinThreshold (outputP0,outputP1) (outputP0Old,outputP1Old) &&
       isConstantOverTime (outputP0,outputP1) ((outputP0Old,outputP1Old):oldSignal) then
           if counter >= fromInteger cycleLimit then
                (fromInteger erroneousState, newOldSignal, fromInteger cycleLimit, 0)
            else
                if recoveryCounter > 0 then
                    (fromInteger erroneousState, newOldSignal, fromInteger cycleLimit, recoveryCounter - 1)
                else
                    (fromInteger correctState, newOldSignal, counter + 1, 0)
    else
        if recoveryCounter > 0 then
            (fromInteger recoveryState, newOldSignal, 0, recoveryCounter - 1)
        else
            (fromInteger correctState, newOldSignal, 0, 0)

