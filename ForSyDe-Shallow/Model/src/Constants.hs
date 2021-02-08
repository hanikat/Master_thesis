--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Tue Dec 29 2020
-- Contact: hanikat@kth.se
-- Copyright: (c) 2020 Marcus Hanikat
--
-- Description: Constants used in program flow.
--------------------------------------------------
module Constants where

-- Constants
tc = 50
constantA = [1,2]
constantB = 500
constantC = 500
constantD = 500
constantE = 500
constantF = 500
constantG = 500
constantH = constantE
constantI = 500



-- States which each verification of the application can be within
correctState = 0
erroneousState :: Integer
erroneousState = 1
recoveryState = 2



constantJ = 500
comstantK = 500
constantL = 500
constantM = 500
constantN = 0.35
constantO = 500


-- Help functions for parsing a 4-way tuple
first :: (Int,Int,Int,Int) -> Int 
first (x,_,_,_) = x

second :: (Int,Int,Int,Int) -> Int 
second (_,x,_,_) = x

third :: (Int,Int,Int,Int) -> Int 
third (_,_,x,_) = x

fourth :: (Int,Int,Int,Int) -> Int 
fourth (_,_,_,x) = x