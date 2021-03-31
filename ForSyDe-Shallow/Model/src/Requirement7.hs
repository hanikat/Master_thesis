--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Mon Dec 28 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Input-Dependencies: Requirement 2, SignalB, SignalC
-- Output-Dependencies: Requirement 8
-- Requirement: Requirement 7
-- Description: Requirement7
-- Re-use of the requirement5 function since the same calculations are performed
--------------------------------------------------
module Requirement7 where

import Requirement5


-- Input  [((outputB[0][0],outputB[0][1],outputB[1][0],outputB[1][1]),(outputC[0],outputC[1]))] ->
--        [(signalB[0],signalB[1])] ->
--        [(signalC[0][0],signalC[0][1],signalC[1][0],signalC[1][1])]
-- Output [(outputJ[0][0],outputJ[0][1],outputJ[1][0],outputJ[1][1])]
requirement7 :: (Fractional a, Ord a) => [(Int,Int,Int,Int)] -> [(Int,Int)] -> [(Int,Int,Int,Int)] -> [(a,a,a,a)]
requirement7 [] _ _ = []
requirement7 _ [] _ = []
requirement7 _ _ [] = []
requirement7 outputB signalB signalC = requirement5 outputB (map requirement7Helper signalB) (map requirement7Helper2 signalC)

requirement7Helper :: (Int,Int) -> (Int,Int)
requirement7Helper (signalB0,signalB1) = (negate signalB0, negate signalB1)

requirement7Helper2 :: (Int,Int,Int,Int) -> (Int,Int,Int,Int)
requirement7Helper2 (signalC00,signalC01,signalC10,signalC11) = (negate signalC00, negate signalC01, negate signalC10, negate signalC11)