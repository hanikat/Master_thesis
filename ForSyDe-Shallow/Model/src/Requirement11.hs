--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Fri Jan 01 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Input-Dependencies: Requirement 8
-- Output-Dependencies: Requirement 15
-- Requirement: Requirement 11
-- Description: Requirement 11
--------------------------------------------------
module Requirement11 where

import Requirement10

-- Input [(outputK[0][0],outputK[0][1],outputK[1][0],outputK[1][1])]
-- Output [(outputO[0],outputO[1])]
-- Re-using requirement 10 since the same calculation is performed
requirement11 :: (Fractional a, Ord a) => [(a,a,a,a)] -> [(a,a)]
requirement11 ((outputK00,outputK01,outputK10,outputK11):remainingOutputK) =
    (min outputK00 outputK01, min outputK10 outputK11) : requirement10 remainingOutputK