--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Sat Jan 02 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Description: Requirement 12
--------------------------------------------------
module Requirement12 where

import Constants

-- Input  [(outputP[0](t-1),outputP[1](t-1)] -> 
--        [(outputQ[0](t-1),outputQ[1](t-1))] -> 
--        [(outputM[0],outputM[1])]
-- Output ([(outputP[0],outputP[1])],[(outputQ[0],outputQ[1])])
requirement12 :: (Fractional a, Ord a) => [(a,a)] -> [(a,a)] -> [(a,a)] -> ([(a,a)],[(a,a)])
requirement12 [] _ _ = ([],[])
requirement12 _ [] _ = ([],[])
requirement12 _ _ [] = ([],[])
requirement12 ((outputP0Old,outputP1Old):remainingOutputP) ((outputQ0Old,outputQ1Old):remainingOutputQ) ((outputM0,outputM1):remainingoutputM) = do
    let outputP = (fst computeIndex0, fst computeIndex1)
    let outputQ = (snd computeIndex0, snd computeIndex1)
    (outputP : (fst nextOutput), outputQ : (snd nextOutput))
    where {
        computeIndex0 = requirement12Compute (outputM0, outputP0Old, outputQ0Old);
        computeIndex1 = requirement12Compute (outputM1, outputP1Old, outputQ1Old);
        nextOutput = requirement12 remainingOutputP remainingOutputQ remainingoutputM;
    }


-- Input [(outputM[x],outputP[x](t-1),outputQ[x](t-1))]
-- Output [(outputP[x],outputQ[x])]
requirement12Compute :: (Fractional a, Ord a) => (a,a,a) -> (a,a)
requirement12Compute (outputM,outputPOld,outputQOld) =
    (outputP, outputQ)
    where {
        outputP = numerator / fromInteger (constantC + constantD);
        outputQ = numerator - fromInteger (constantC + constantD) * outputP;
        numerator = fromInteger constantD * outputPOld + fromInteger constantC * outputM + outputQOld;
    }