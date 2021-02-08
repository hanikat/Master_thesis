--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Mon Jan 11 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Description: Verification3
--------------------------------------------------
module Verification3 where

import Constants

-- Input [(outputY[0], outputY[1])]
-- Output [states]
verification3 :: [(Int,Int)] -> [Int]
verification3 [] = []
verification3 ((outputY0, outputY1):remainingSignal) = 
    if min outputY0 outputY1 >= 0 || max outputY0 outputY1 < 0 then
        fromInteger correctState : verification3 remainingSignal
    else
        fromInteger erroneousState : verification3 remainingSignal