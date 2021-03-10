--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Fri Jan 01 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Description: Requirement 10
--------------------------------------------------
module Requirement10 where

-- Input [(outputH[0][0],outputH[0][1],outputH[1][0],outputH[1][1])]
-- Output [(outputN[0],outputN[1])]
requirement10 :: (Fractional a, Ord a) => [(a,a,a,a)] -> [(a,a)]
requirement10 [] = []
requirement10 ((outputH00,outputH01,outputH10,outputH11):remainingOutputH) =
    (max outputH00 outputH01, max outputH10 outputH11) : requirement10 remainingOutputH
    