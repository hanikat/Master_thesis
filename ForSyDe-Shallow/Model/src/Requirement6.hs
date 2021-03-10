--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Mon Dec 28 2020
-- Contact: hanikat@kth.se
-- Copyright: (c) 2020 Marcus Hanikat
--
-- Description: Requirement 6
--------------------------------------------------
module Requirement6 where

import Constants
import Utility

-- Input [(outputH[0][0](t-1),outputH[0][1](t-1),outputH[1][0](t-1),outputH[1][1](t-1))] -> 
--       [(outputI[0][0](t-1),outputI[0][1](t-1),outputI[1][0](t-1),outputI[1][1](t-1))] ->
--       [((outputB[0][0],outputB[0][1],outputB[1][0],outputB[1][1]))] -> 
--       [(outputG[0][0],outputG[0][1],outputG[1][0],outputG[1][1])]

-- Output [((outputH[0][0],outputH[0][1],outputH[1][0],outputH[1][1]),(outputI[0][0],outputI[0][1],outputI[1][0],outputI[1][1]))]
requirement6 :: (Fractional a, Ord a) => [(a,a,a,a)] -> [(a,a,a,a)] -> [(Int,Int,Int,Int)] -> [(a,a,a,a)] -> ([(a,a,a,a)], [(a,a,a,a)])
requirement6 [] _ _ _ = ([],[])
requirement6 _ [] _ _ = ([],[])
requirement6 _ _ [] _ = ([],[])
requirement6 _ _ _ [] = ([],[])
requirement6 (outputH:remainingOutputH) (outputI:remainingOutputI) (outputB:remainingOutputB) (outputG:remainingOutputG) = do
   
   let outputH00 = fst computeIndex00;
   let outputH01 = fst computeIndex01;
   let outputH10 = fst computeIndex10;
   let outputH11 = fst computeIndex11;
   let outputI00 = snd computeIndex00;
   let outputI01 = snd computeIndex01;
   let outputI10 = snd computeIndex10;
   let outputI11 = snd computeIndex11;
   
   ((outputH00,outputH01,outputH10,outputH11) : (fst nextOutput), (outputI00,outputI01,outputI10,outputI11) : (snd nextOutput))

   where {
      computeIndex00 = requirement6Compute (first outputB, first outputG, first outputH , first outputI);
      computeIndex01 = requirement6Compute (second outputB, second outputG, second outputH , second outputI);
      computeIndex10 = requirement6Compute (third outputB, third outputG, third outputH, third outputI);
      computeIndex11 = requirement6Compute (fourth outputB, fourth outputG, fourth outputH , fourth outputI);

      nextOutput = requirement6 remainingOutputH remainingOutputI remainingOutputB remainingOutputG
   }



-- Input [(outputB[x][y], outputG[x][y], outputHOld[x][y](t-1), outputIOld[x][y](t-1))]
-- Output [(outputH,outputI)]
requirement6Compute :: (Fractional a, Eq a, Ord a) =>  (Int,a,a,a) -> (a,a)
requirement6Compute (outputB,outputG,outputHOld,outputIOld) =
     if outputB /= 0 then 
        (outputG, outputI)
     else
         (x, y)
         where {
            -- x = outputH
            x = if outputIOld > 0 then
                   outputHOld
                else
                    0;
            -- y = outputI
            y = if outputG == 0 then
                    max (outputIOld - abs (x * fromIntegral constantC)) 0;
                else
                    outputI;
            -- outputI calculation
            outputI = (abs (outputG * realToFrac constantI) + 50) / 100;
         }