--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Tue Dec 29 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Description: 
--------------------------------------------------
module Requirement8 where

-- Re-use requirement6 since the same calculations are performed, just on different signals
import Constants
import Requirement6


-- Input [(outputK[0][0](t-1), outputK[0][1](t-1), outputK[1][0](t-1), outputK[1][1](t-1))] -> 
--       [(outputL[0][0](t-1), outputL[0][1](t-1), outputL[1][0](t-1), outputL[1][1](t-1))] ->
--       [(outputB[0][0],outputB[0][1],outputB[1][0],outputB[1][1])] -> 
--       [(outputJ[0][0],outputJ[0][1],outputJ[1][0],outputJ[1][1])]

-- Output ([(outputK[0][0],outputK[0][1],outputK[1][0],outputK[1][1])],[(outputL[0][0],outputL[0][1],outputL[1][0],outputL[1][1])])
requirement8 :: [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)] -> ([(Int,Int,Int,Int)],[(Int,Int,Int,Int)])
requirement8 [] _ _ _ = ([],[])
requirement8 _ [] _ _ = ([],[])
requirement8 _ _ [] _ = ([],[])
requirement8 _ _ _ [] = ([],[])
requirement8 (outputK:remainingOutputK) (outputL:remainingOutputL) (outputB:remainingOutputB) (outputJ:remainingOutputJ) = do
   
   let outputK00 = fst computeIndex00
   let outputK01 = fst computeIndex01
   let outputK10 = fst computeIndex10
   let outputK11 = fst computeIndex11
   let outputL00 = snd computeIndex00
   let outputL01 = snd computeIndex01
   let outputL10 = snd computeIndex10
   let outputL11 = snd computeIndex11

   ((outputK00,outputK01,outputK10,outputK11) : (fst nextOutput), (outputL00,outputL01,outputL10,outputL11) : (snd nextOutput))

   where {
      computeIndex00 = requirement6Compute (first outputB, first outputJ, first outputK, first outputL);
      computeIndex01 = requirement6Compute (second outputB, second outputJ, second outputK, second outputL);
      computeIndex10 = requirement6Compute (third outputB, third outputJ, third outputK, third outputL);
      computeIndex11 = requirement6Compute (fourth outputB, fourth outputJ, fourth outputK, fourth outputL);

      nextOutput = requirement8 remainingOutputK remainingOutputL remainingOutputB remainingOutputJ;
   }
