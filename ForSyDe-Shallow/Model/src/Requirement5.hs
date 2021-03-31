--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Sat Dec 26 2020
-- Contact: hanikat@kth.se
-- Copyright: (c) 2020 Marcus Hanikat
--
-- Input-Dependencies: Requirement2, SignalB, SignalC
-- Output-Dependencies: Requirement 6
-- Requirement: Requirement 5
-- Description: Requirement 5
-- Re-use of the requirement3 function since the same calculations are performed
--------------------------------------------------
module Requirement5 where

import Requirement3

-- Input [(outputB[0][0],outputB[0][1],outputB[1][0],outputB[1][1])] ->
--       [(signalB[0],signalB[1])] ->  
--       [(signalC[0][0],signalC[0][1],signalC[1][0],signalC[1][1])]
-- Output [(outputG[0][0],outputG[0][1],outputG[1][0],outputG[1][1])]
requirement5 :: (Fractional a, Ord a) => [(Int,Int,Int,Int)] -> [(Int,Int)] -> [(Int,Int,Int,Int)] -> [(a,a,a,a)]
requirement5 [] _ _ = []
requirement5 _ [] _ = []
requirement5 _ _ [] = []
requirement5 ((outputB00,outputB01,outputB10,outputB11):remainingOutputB) ((signalB0,signalB1):remainingSignalB) ((signalC00,signalC01,signalC10,signalC11):remainingSignalC) = 
    (requirement3Helper (fromInteger outputG00), requirement3Helper (fromInteger outputG01), requirement3Helper (fromInteger outputG10), requirement3Helper (fromInteger outputG11)) : (requirement5 remainingOutputB remainingSignalB remainingSignalC)
    where {
        outputG00 = toInteger (signalC00 + outputB00 + (2 * signalB0));
        outputG01 = toInteger (signalC01 + outputB01 + (2 * signalB0));
        outputG10 = toInteger (signalC10 + outputB10 + (2 * signalB1));
        outputG11 = toInteger (signalC11 + outputB11 + (2 * signalB1));
    }
