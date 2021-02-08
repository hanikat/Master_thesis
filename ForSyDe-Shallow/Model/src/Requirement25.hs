--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Thu Jan 07 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Description: Requirement 25
--------------------------------------------------
module Requirement25 where

import Constants


-- Input  [(outputB[0][0],outputB[0][1],outputB[1][0],outputB[1][1])] -> 
--        [(outputC[0],outputC[1])] ->
--        [(outputAA[0](t-1),outputAA[1](t-1))]
-- Output ([(outputZ[0],outputZ[1])],[(outputAA[0],outputAA[1])])
requirement25 :: [(Int,Int,Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> ([(Int,Int)],[(Int,Int)])
requirement25 [] _ _ = ([],[])
requirement25 _ [] _ = ([],[])
requirement25 _ _ [] = ([],[])
requirement25 (outputB:remainingOutputB) (outputC:remainingOutputC) (outputAA:remainingOutputAA) = do
    let outputZ = (snd computeIndex0, snd computeIndex1)
    let outputAA = (snd computeIndex0, snd computeIndex1)
    (outputZ : (fst nextOutput), outputAA : (snd nextOutput))
    where {
        computeIndex0 = requirement25Compute (first outputB, fst outputC, fst outputAA);
        computeIndex1 = requirement25Compute (third outputB, snd outputC, snd outputAA);
        nextOutput = requirement25 remainingOutputB remainingOutputC remainingOutputAA;
    }


-- Input  (outputB[x][0],outputC[x],outputAA[x])
-- Output (outputZ[x], outputAA[x])
requirement25Compute :: (Int,Int,Int) -> (Int,Int)
requirement25Compute (outputB,outputC,outputAAOld) = 
    (fromInteger outputZ, fromInteger outputAA)
    where {
        numerator = constantG * constantB * toInteger outputB * toInteger outputC + toInteger outputAAOld;
        denominator = constantH * constantC;
        outputZ = numerator `div` denominator;
        outputAA = numerator - (outputZ * denominator);
    }