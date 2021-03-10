--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Sun Jan 03 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Description: Requirement 14
--------------------------------------------------
module Requirement14 where

import Requirement12

-- Input [(outputR[0](t-1),outputR[1](t-1)] -> 
--       [(outputS[0](t-1),outputS[1](t-1))] -> 
--       [(outputN[0],outputN[1])]
-- Output ([(outputR[0],outputR[1])], [(outputS[0],outputS[1])])
-- Re-using requirement12 since the same calculations are performed
requirement14 :: (Fractional a, Ord a) => [(a,a)] -> [(a,a)] -> [(a,a)] -> ([(a,a)],[(a,a)])
requirement14 outputR outputS outputN = requirement12 outputR outputS outputN