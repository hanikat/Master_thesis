--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Thu Jan 07 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Description: Requirement24
--------------------------------------------------
module Requirement24 where




-- Input [(outputX[0][0], outputX[0][1], outputX[1][0], outputX[1][1])]
-- Output [outputY[x]]
requirement24 :: (Fractional a, Ord a) => [(a,a,a,a)] -> [(a,a)]
requirement24 [] = []
requirement24 ((outputX00,outputX01,outputX10,outputX11):remainingSignal) = 
    (outputY0,outputY1) : requirement24 remainingSignal
    where {
        outputY0 = (outputX00 + outputX01 + 1) / 2;
        outputY1 = (outputX10 + outputX11 + 1) / 2;
    }