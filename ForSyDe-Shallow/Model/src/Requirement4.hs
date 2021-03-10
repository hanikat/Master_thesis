--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Sat Dec 26 2020
-- Contact: hanikat@kth.se
-- Copyright: (c) 2020 Marcus Hanikat
--
-- Description: Requirement 4
--------------------------------------------------
module Requirement4 where

import Constants
import Utility
-- Input  [(outputE[0][0](t-1),outputE[0][1](t-1),outputE[1][0](t-1),outputE[1][1](t-1))] -> 
--        [(outputF[0][0](t-1),outputF[0][1](t-1),outputF[1][0](t-1),outputF[1][1](t-1))] ->
--        [(outputB[0][0],outputB[0][1],outputB[1][0],outputB[1][1],)] ->
--        [(outputD[0][0],outputD[0][1],outputD[1][0],outputD[1][1],)]
-- Output ([(outputE[0][0],outputE[0][1],outputE[1][0],outputE[1][1])], [(outputF[0][0],outputF[0][1],outputF[1][0],outputF[1][1])])
requirement4 :: (Fractional a, Ord a) => [(a,a,a,a)] -> [(a,a,a,a)] -> [(Int,Int,Int,Int)] -> [(a,a,a,a)] -> ([(a,a,a,a)], [(a,a,a,a)])
requirement4 [] _ _ _ = ([],[])
requirement4 _ [] _ _ = ([],[])
requirement4 _ _ [] _ = ([],[])
requirement4 _ _ _ [] = ([],[])
requirement4 (outputE:remainingOutputE) (outputF:remainingOutputF) (outputB:remainingOutputB) (outputD:remainingOutputD) = do
   let outputE00 = fst computeIndex00
   let outputE01 = fst computeIndex01
   let outputE10 = fst computeIndex10
   let outputE11 = fst computeIndex11
   let outputF00 = snd computeIndex00
   let outputF01 = snd computeIndex01
   let outputF10 = snd computeIndex10
   let outputF11 = snd computeIndex11

   ((outputE00,outputE01,outputE10,outputE11) : (fst nextOutput), (outputF00,outputF01,outputF10,outputF11) : (snd nextOutput))

   where {
      computeIndex00 = requirement4Compute (first outputB, first outputD, first outputE, first outputF);
      computeIndex01 = requirement4Compute (second outputB, second outputD, second outputE, second outputF);
      computeIndex10 = requirement4Compute (third outputB, third outputD, third outputE, third outputF);
      computeIndex11 = requirement4Compute (fourth outputB, fourth outputD, fourth outputE, fourth outputF);
      nextOutput = requirement4 remainingOutputE remainingOutputF remainingOutputB remainingOutputD  
   }


-- Input [(outputB[x][y], outputD[x][y], outputE[x][y](t-1), outputF[x][y](t-1))]
-- Output (outputE[x][y], outptutF[x][y])
requirement4Compute :: (Fractional a, Ord a) => (Int,a,a,a) -> (a,a)
requirement4Compute (outputB,outputD,outputEOld,outputFOld) =
     if outputB /= 0 then 
        (outputD, outputF)
     else
         (x, y)
         where {
            x = if outputFOld > 0 then
                   outputEOld
                else
                    0;
            y = max (outputF - abs (x * fromIntegral constantC)) 0;
            outputF = (abs (outputD * realToFrac constantI) + 50) / 100;
         }