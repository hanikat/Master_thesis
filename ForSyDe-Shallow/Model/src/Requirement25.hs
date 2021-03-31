--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Thu Jan 07 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Input-Dependencies: Requirement 2
-- Output-Dependencies: Requirement 26
-- Requirement: Requirement 25
-- Description: Requirement 25
--------------------------------------------------
module Requirement25 where

import Constants ( constantC, constantH, constantB, constantG )
import Utility

-- Input  [(outputB[0][0],outputB[0][1],outputB[1][0],outputB[1][1])] -> 
--        [(outputC[0],outputC[1])] ->
--        [(outputAA[0](t-1),outputAA[1](t-1))]
-- Output ([(outputZ[0],outputZ[1])],[(outputAA[0],outputAA[1])])
requirement25 :: (Fractional a, Ord a) => [(Int,Int,Int,Int)] -> [(Int,Int)] -> [(a,a)] -> ([(a,a)],[(a,a)])
requirement25 [] _ _ = ([],[])
requirement25 _ [] _ = ([],[])
requirement25 _ _ [] = ([],[])
requirement25 (outputB:remainingOutputB) (outputC:remainingOutputC) (outputAA:remainingOutputAA) = do
    let outputZ = (fst computeIndex0, fst computeIndex1)
    let outputAA = (snd computeIndex0, snd computeIndex1)
    (outputZ : (fst nextOutput), outputAA : (snd nextOutput))
    where {
        computeIndex0 = requirement25Compute (first outputB, fst outputC, fst outputAA);
        computeIndex1 = requirement25Compute (third outputB, snd outputC, snd outputAA);
        nextOutput = requirement25 remainingOutputB remainingOutputC remainingOutputAA;
    }


-- Input  (outputB[x][0],outputC[x],outputAA[x])
-- Output (outputZ[x], outputAA[x])
requirement25Compute :: (Fractional a, Ord a) => (Int,Int,a) -> (a,a)
requirement25Compute (outputB,outputC,outputAAOld) = 
    (outputZ, outputAA)
    where {
        numerator = fromInteger constantG * fromInteger constantB * fromInteger (toInteger outputB) * fromInteger (toInteger outputC) + outputAAOld;
        denominator = fromInteger constantH * fromInteger constantC;
        outputZ = numerator / denominator;
        outputAA = numerator - (outputZ * denominator);
    }
