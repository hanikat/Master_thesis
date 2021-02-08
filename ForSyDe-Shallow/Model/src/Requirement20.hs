--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Tue Jan 05 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Description: Requirement20
--------------------------------------------------
module Requirement20 where

-- Input [(outputH[0],outputH[1])] -> 
--       [(outputP[0],outputP[1])]
-- Output [(outputV[0],outputV[1])]
requirement20 :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
requirement20 [] _ = []
requirement20 _ [] = []
requirement20 ((outputH0,outputH1):remainingOutputH) ((outputP0,outputP1):remainingOutputP) =
    (requirement20Helper outputV0, requirement20Helper outputV1) : requirement20 remainingOutputH remainingOutputP
    where {
        outputV0 = outputP0 * outputH0;
        outputV1 = outputP1 * outputH1;
    }

requirement20Helper :: Int -> Int
requirement20Helper x =
    if x > 0 then
        1
    else if x < 0 then
        -1
    else
        0
