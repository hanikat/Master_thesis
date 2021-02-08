--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Mon Jan 11 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Description: Verification 4
--------------------------------------------------
module Verification4 where

import Constants

-- Input [(outputP[0], outputP[1])]
-- Output [states]
verification4 :: [(Int,Int)] -> [Int]
verification4 [] = []
verification4 ((outputP0, outputP1):remainingSignal) = 
    if abs outputP0 > fromIntegral comstantK || abs outputP1 > fromIntegral comstantK then
        fromInteger erroneousState : verification4 remainingSignal
    else
        fromInteger correctState : verification4 remainingSignal