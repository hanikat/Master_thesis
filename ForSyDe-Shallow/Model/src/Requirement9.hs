--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Tue Dec 29 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Description: Requirement 9
--------------------------------------------------
module Requirement9 where

-- Input [(outputE[0][0],outputE[0][1],outputE[1][0],outputE[1][1])]
-- Output [(outputM[0],outputM[1])]
requirement9 :: [(Int,Int,Int,Int)] -> [(Int,Int)]
requirement9 [] = []
requirement9 ((outputE00,outputE01,outputE10,outputE11):remainingoutputE) = 
    (requirement9Helper (outputE00,outputE01), requirement9Helper (outputE10,outputE11)) : requirement9 remainingoutputE

-- Input [(outputE[x][0],outputE[x][1])]
-- Output [outputM[x]]
requirement9Helper :: (Integral a, Num a) => (a,a) -> a
requirement9Helper (outputE0,outputE1) = 
    if x >= 0 then
        (x + 1) `div` 2
    else
        (x - 1) `div` 2
    where {
        x = outputE0 + outputE1;
    }