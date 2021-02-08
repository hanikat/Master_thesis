--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Fri Dec 25 2020
-- Contact: hanikat@kth.se
-- Copyright: (c) 2020 Marcus Hanikat
--
-- Description: Requirement 3
--------------------------------------------------
module Requirement3 where

import Constants

-- Input [((outputB[0][0],outputB[0][1],outputB[1][0],outputB[1][1]))]
-- Output [(outputD[0][0],outputD[0][1],outputD[1][0],outputD[1][1])]
requirement3 :: [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)]
requirement3 [] = []
requirement3 ((outputB00,outputB01,outputB10,outputB11):remainingOutput) = 
   (requirement3Helper outputB00, requirement3Helper outputB01, requirement3Helper outputB10, requirement3Helper outputB11) : requirement3 remainingOutput

requirement3Helper :: Int -> Int
requirement3Helper x
     | d >= 0 = fromIntegral ((d + (e `div` 2)) `div` e)
     | otherwise = fromIntegral ((d - (e `div` 2)) `div` e)
     where {
        d = constantD * toInteger x * constantB;
        e = constantE * constantC * constantI;
     }