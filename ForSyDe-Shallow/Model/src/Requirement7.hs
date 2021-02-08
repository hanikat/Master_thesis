--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Mon Dec 28 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Description: Requirement7
-- Re-use of the requirement5 function since the same calculations are performed
--------------------------------------------------
module Requirement7 where

import Requirement5


-- Input  [((outputB[0][0],outputB[0][1],outputB[1][0],outputB[1][1]),(outputC[0],outputC[1]))] ->
--        [(signalB[0],signalB[1])] ->
--        [(signalC[0][0],signalC[0][1],signalC[1][0],signalC[1][1])]
-- Output [(outputJ[0][0],outputJ[0][1],outputJ[1][0],outputJ[1][1])]
requirement7 ::  [(Int,Int,Int,Int)] -> [(Int,Int)] -> [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)]
requirement7 [] _ _ = []
requirement7 _ [] _ = []
requirement7 _ _ [] = []
requirement7 outputB signalB signalC = requirement5 outputB (map requirement7Helper signalB) signalC

requirement7Helper :: (Int,Int) -> (Int,Int)
requirement7Helper (signalB0,signalB1) = (negate signalB0, negate signalB1)