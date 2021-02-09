--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Tue Jan 12 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Description: Verification 5
--------------------------------------------------
module Verification5 where

import Constants

cycleLimit = toInteger (30 * (1000 `div` fromIntegral tc))

-- Helper functions
-- Input (outputP[0], outputP[1])
-- Output Bool
isNonZero :: (Int,Int) -> Bool
isNonZero (outputP0, outputP1) =
    if abs outputP0 > 0 && abs outputP1 > 0 then
        True 
    else
        False

-- Input (outputP[0], outputP[1]) -> 
--       (outputP[0](t-1), outputP[1](t-1))
-- Output Bool
isWithinThreshold :: (Int,Int) -> (Int,Int) -> Bool 
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
isConstantOverTime' :: Int -> [Int] -> Bool
isConstantOverTime' _ [] = True
isConstantOverTime' currentValue (previousValue:remainingListOfPreviousValues) = 
    if abs (currentValue - previousValue) > fromInteger constantM then
        False 
    else 
        isConstantOverTime' currentValue remainingListOfPreviousValues

-- Input (outputP[0], outputP[1]) -> 
--       [(outputP[0](t-1),outputP[0](t-1))...(outputP[0](t-cycleLimit),outputP[0](t-cycleLimit))]
-- Output Bool, Used to indicate if all values are within range for the instance with the current highest value
isConstantOverTime :: (Int,Int) -> [(Int,Int)] -> Bool 
isConstantOverTime (outputP0,outputP1) listOfPreviousValues = 
    if outputP0 > outputP1 then
        isConstantOverTime' outputP0 (map fst (take (fromInteger cycleLimit) listOfPreviousValues))
    else
        isConstantOverTime' outputP1 (map snd (take (fromInteger cycleLimit) listOfPreviousValues))


-- Input  [(outputP[0], outputP[1])] ->
--        [(outputP[0](t-1),outputP[0](t-1))...(outputP[0](t-cycleLimit),outputP[0](t-cycleLimit))]
-- Output ([state],[(outputP[0],outputP[0])...(outputP[0](t-cycleLimit+1),outputP[0](t-cycleLimit+1))],[counter])
verification5 :: [(Int,Int)] -> [(Int,Int)] -> [Int] -> ([Int],[(Int,Int)],[Int])
verification5 [] _ _ = ([],[],[])
verification5 _ [] _ = ([],[],[])
verification5 _ _ [] = ([],[],[])
verification5 (outputP:remainingOutputP) outputPOld (currentCounter:remainingCounters) = do
    ((state : nextState), (outputP : nextOldOutputP), (counter : nextCounter))
    where {
        (state, oldOutputP,counter) = verification5Helper outputP  outputPOld currentCounter;
        (nextState, nextOldOutputP, nextCounter) = verification5 remainingOutputP oldOutputP remainingCounters;
    }
    

-- Input (outputP[0], outputP[1]) ->
--       [(outputP[0](-t), outputP[1](-t))] ->
--       [counter]
-- Output [state]
verification5Helper :: (Int, Int) -> [(Int, Int)] -> Int -> (Int,[(Int,Int)],Int)
verification5Helper (outputP0, outputP1) ((outputP0Old,outputP1Old):oldSignal) counter = do
    let newOldSignal = ((outputP0,outputP1) : (outputP0Old,outputP1Old) : init oldSignal)
    if isNonZero (outputP0,outputP1) &&
       isWithinThreshold (outputP0,outputP1) (outputP0Old,outputP1Old) &&
       isConstantOverTime (outputP0,outputP1) ((outputP0Old,outputP1Old):oldSignal) then
           (fromInteger erroneousState, newOldSignal, fromInteger cycleLimit)
    else
        if counter > 0 then
            (fromInteger recoveryState, newOldSignal, counter - 1)
        else
            (fromInteger correctState, newOldSignal, 0)

