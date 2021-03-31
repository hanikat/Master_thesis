--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Tue Jan 05 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Input-Dependencies: Requirement 14, Requirement 15, Requirement 20
-- Output-Dependencies: Requirement 2
-- Requirement: Requirement 21
-- Description: Requirement 21
--------------------------------------------------
module Requirement21 where

-- Input [(outputR[0],outputR[1])] -> 
--       [(outputT[0],outputT[1])] -> 
--       [(outputV[0],outputV[1])]
-- Output [(outputW[0],outputW[1])]
requirement21 :: (Fractional a, Ord a) => [(a,a)] -> [(a,a)] -> [(Int,Int)] -> [(Int,Int)]
requirement21 [] _ _ = []
requirement21 _ [] _ = []
requirement21 _ _ [] = []
requirement21 ((outputR0,outputR1):remainingOutputR) ((outputT0,outputT1):remainingOutputT) ((outputV0,outputV1):remainingOutputV) = 
   (requirement21Helper (outputR0,outputT0,outputV0), requirement21Helper (outputR1,outputT1,outputV1)) : requirement21 remainingOutputR remainingOutputT remainingOutputV 

-- Input [(outputR[x], outputT[x], outputV[x])]
-- Output [outputW[x]]
requirement21Helper :: (Fractional a, Ord a) => (a,a,Int) -> Int
requirement21Helper (outputR,outputT,outputV) = 
    if (outputR > 0 && outputT < 0) || (outputR < 0 && outputT > 0) || (outputR == 0 || outputT == 0) then
        0
    else
        outputV
