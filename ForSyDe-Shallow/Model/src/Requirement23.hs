--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Thu Jan 07 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Description: Requirement 23
--------------------------------------------------
module Requirement23 where

import Constants

-- Input  [(outputE[0][0](t-2),outputE[0][1](t-2),outputE[1][0](t-2),outputE[1][1](t-2))] -> 
--        [(outputE[0][0],outputE[0][1],outputE[1][0],outputE[1][1],outputE[1][1])]
-- Output [(outputX[0][0],outputX[0][1],outputX[1][0],outputX[1][1])]
requirement23 :: [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)]
requirement23 [] _ = []
requirement23 _ [] = []
requirement23 ((outputE00Old,outputE01Old,outputE10Old,outputE11Old):remainingOutputEOld) ((outputE00,outputE01,outputE10,outputE11):remainingOutputE) = do
    let output = (requirement23Compute (outputE00,outputE00Old), requirement23Compute (outputE01,outputE01Old), requirement23Compute (outputE10,outputE10Old), requirement23Compute (outputE11,outputE11Old))
    output : requirement23 remainingOutputEOld remainingOutputE


-- Input [(outputE[x][y], outputE[x][y](t-2))]
-- Output [outputX]
requirement23Compute :: (Integral a, Num a) => (a,a) -> a
requirement23Compute (outputE,outputEold) = outputX
    where {
        outputX = (200000 * (abs outputE - abs outputEold) + fromInteger constantF) `div` (2 * fromInteger constantF);
    }
