--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Tue Jan 12 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Description: Verification 6
--------------------------------------------------
module Verification6 where

import Constants

cycleLimit = toInteger (verification6Limit * (500 `div` fromIntegral tc))
threshold = fromInteger cycleLimit * constantN


-- Helper function
-- Input [verification5States]
-- Output Bool, returns true if the number of occurances of erroneousState in list is less than the threshold
checkIfLowerThanThreshold :: [Int] -> Bool
checkIfLowerThanThreshold verification5States = 
    if (length . filter (== erroneousState)) (map toInteger verification5States) > round threshold then
        False
    else
        True


-- Input [verification5States]
-- Output [verification6States]
verification6 :: [Int] -> [Int]
verification6 [] = []
verification6 verification5States = 
    verification6Helper verification5States

-- Input [verification5States]
-- Output [verification6States]
verification6Helper :: [Int] -> [Int]
verification6Helper [] = []
verification6Helper (currentVerification5State:remainingVerification5States) = do
    if checkIfLowerThanThreshold (take (round threshold) (currentVerification5State:remainingVerification5States)) then
            fromInteger correctState : verification6Helper remainingVerification5States
    else
        repeat (fromInteger erroneousState)
