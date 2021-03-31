--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Tue Jan 05 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Input-Dependencies: Requirement 2, Requirement 12
-- Output-Dependencies: Requirement 21
-- Requirement: Requirement 20
-- Description: Requirement20
--------------------------------------------------
module Requirement20 where

-- Input [(outputC[0],outputC[1])] -> 
--       [(outputP[0],outputP[1])]
-- Output [(outputV[0],outputV[1])]
requirement20 :: (Fractional a, Ord a) => [(Int,Int)] -> [(a,a)] -> [(Int,Int)]
requirement20 [] _ = []
requirement20 _ [] = []
requirement20 ((outputC0,outputC1):remainingOutputC) ((outputP0,outputP1):remainingOutputP) =
    (requirement20Helper outputV0, requirement20Helper outputV1) : requirement20 remainingOutputC remainingOutputP
    where {
        outputV0 = outputP0 * fromInteger (toInteger outputC0);
        outputV1 = outputP1 * fromInteger (toInteger outputC1);
    }

requirement20Helper :: (Fractional a, Ord a) => a -> Int
requirement20Helper x =
    if x > 0 then
        1
    else if x < 0 then
        -1
    else
        0