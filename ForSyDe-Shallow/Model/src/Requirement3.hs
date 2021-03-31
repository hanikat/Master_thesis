--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Fri Dec 25 2020
-- Contact: hanikat@kth.se
-- Copyright: (c) 2020 Marcus Hanikat
--
-- Input-Dependencies: Requirement 2
-- Output-Dependencies: Requirement 4
-- Requirement: Requirement 3
-- Description: Requirement 3
--------------------------------------------------
module Requirement3 where

import Constants

-- Input [((outputB[0][0],outputB[0][1],outputB[1][0],outputB[1][1]))]
-- Output [(outputD[0][0],outputD[0][1],outputD[1][0],outputD[1][1])]
requirement3 :: (Fractional a, Ord a) => [(Int,Int,Int,Int)] -> [(a,a,a,a)]
requirement3 [] = []
requirement3 ((outputB00,outputB01,outputB10,outputB11):remainingOutput) = 
   (requirement3Helper (fromInteger(toInteger(outputB00))), requirement3Helper (fromInteger(toInteger(outputB01))), requirement3Helper (fromInteger(toInteger(outputB10))), requirement3Helper (fromInteger(toInteger(outputB11)))) : requirement3 remainingOutput

requirement3Helper :: (Fractional a, Ord a) => a -> a
requirement3Helper x
     | numerator >= 0 = ((numerator + halfDenominator) / denominator)
     | otherwise = ((numerator - halfDenominator) / denominator)
     where {
        numerator = fromInteger(constantD) * x * fromInteger(constantB);
        denominator = fromInteger(constantE) * fromInteger(constantC) * realToFrac constantI;
        halfDenominator = (denominator / 2);
     }