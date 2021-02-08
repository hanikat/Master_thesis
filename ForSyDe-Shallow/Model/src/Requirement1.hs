--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Fri Dec 25 2020
-- Contact: hanikat@kth.se
-- Copyright: (c) 2020 Marcus Hanikat
--
-- Description: Requirement 1 definition
--------------------------------------------------
module Requirement1 where

import Constants

-- Input (signalA[0][0],signalA[0][1],signalA[1][0],signalA[1][1])
-- Output (outputA[0][0],outputA[0][1],outputA[1][0],outputA[1][1])
requirement1 :: [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)]
requirement1 [] = []
requirement1 (x:xs) = requirement1Helper x : requirement1 xs

-- Requirement 1 - Negate values for signalA[x][0] and signalA[x][1] if constantA[x] == 2
requirement1Helper :: (Int,Int,Int,Int) -> (Int,Int,Int,Int)
requirement1Helper (signalA00,signalA01,signalA10,signalA11) = do
        if (constantA !! 0) == 2 && (constantA !! 1) == 2 then
             (negate signalA00, negate signalA01, negate signalA10, negate signalA11)
        else if (constantA !! 0) == 2 then
             (negate signalA00, negate signalA01, signalA10, signalA11)
        else if (constantA !! 1) == 2 then
            (signalA00, signalA01, negate signalA10, negate signalA11)
        else
            (signalA00,signalA01,signalA10,signalA11)
