--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Fri Dec 25 2020
-- Contact: hanikat@kth.se
-- Copyright: (c) 2020 Marcus Hanikat
--
-- Description: Requirement 2
--------------------------------------------------
module Requirement2 where

-- Input [(outputA[0][0],outputA[0][1],outputA[1][0],outputA[1][1])] -> [(outputW[0](t-1),outputW[1](t-1))]
-- Output [((outputB[0][0],outputB[0][1],outputB[1][0],outputB[1][1]),(outputC[0],outputC[1]))]
requirement2 :: [(Int,Int,Int,Int)] -> [(Int,Int)] -> ([(Int,Int,Int,Int)],[(Int,Int)])
requirement2 [] _ = ([],[])
requirement2 _ [] = ([],[])
requirement2 outputA outputW = (map (uncurry requirement2Helper2) $ zip outputA outputW, map (uncurry requirement2Helper3) $ zip outputA outputW)

requirement2Helper2 :: (Int,Int,Int,Int) -> (Int,Int) -> (Int,Int,Int,Int)
requirement2Helper2 (outputA00,outputA01,outputA10,outputA11) (outputW0Old,outputW1Old) = 
   if isOppositeSigns x outputW0Old then
      if isOppositeSigns y outputW1Old then
         (negate outputA00, negate outputA01, negate outputA10, negate outputA11)
      else
         (negate outputA00, negate outputA01, outputA10, outputA11)
   else
      if isOppositeSigns y outputW1Old then
         (outputA00, outputA01, negate outputA10, negate outputA11)
      else
         (outputA00, outputA01, outputA10, outputA11)
   where {
      x = outputA00 + outputA01;
      y = outputA10 + outputA11;
   }

requirement2Helper3 :: (Int,Int,Int,Int) -> (Int,Int) -> (Int,Int)
requirement2Helper3 (outputA00,outputA01,outputA10,outputA11) (outputW0Old,outputW1Old) =
   if isOppositeSigns x outputW0Old then
      if isOppositeSigns y outputW1Old then
         (-1,-1)
      else
         (-1,1)
   else
      if isOppositeSigns y outputW1Old then
         (1,-1)
      else
         (1,1)
   where {
      x = outputA00 + outputA01;
      y = outputA10 + outputA11;
   }
   
isOppositeSigns :: Int -> Int -> Bool
isOppositeSigns x y =
   if (x >= 0 && y >= 0) || (x < 0 && y < 0) then
      False
   else
      True