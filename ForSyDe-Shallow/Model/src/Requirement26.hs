--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Thu Jan 07 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Input-Dependencies: Requirement 25
-- Output-Dependencies: Verification 8
-- Requirement: Requirement 26
-- Description: Requirement 26
--------------------------------------------------
module Requirement26 where

-- Input  [(outputAA[0],outputAA[1])] -> 
--        [(outputAB[0](t-1), outputAB[1](t-1))]
-- Output [(outputAB[0], outputAB[1]]

requirement26 :: (Fractional a, Ord a) => [(a,a)] -> [(a,a)] -> [(a,a)]
requirement26 [] _ = []
requirement26 _ [] = []
requirement26 ((outputAA0, outputAA1):remainingOutputAA) ((outputAB0, outputAB1):remainingOutputAB) =
    (requirement26Compute (outputAA0,outputAB0), requirement26Compute (outputAA1,outputAB1)) : requirement26 remainingOutputAA remainingOutputAB

requirement26Compute :: (Fractional a, Ord a) => (a,a) -> a
requirement26Compute (outputAA, outputABOld) = 
    outputAB 
    where {
        outputAB = 
                outputABOld + outputAA;
    }