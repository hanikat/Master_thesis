--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Sun Jan 03 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Description: Requirement 15
--------------------------------------------------
module Requirement15 where

import Requirement12

-- Input  [(outputT[0](t-1),outputT[1](t-1) ->
--        [(outputU[0](t-1),outputU[1](t-1),))] -> 
--        [(outputO[0],outputO[1])]
-- Output ([(outputT[0],outputT[1])], [(outputU[0],outputU[1])])
-- Re-using requirement12 since the same calculations are performed
requirement15 :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> ([(Int,Int)],[(Int,Int)])
requirement15 outputT outputU outputO = requirement12 outputT outputU outputO