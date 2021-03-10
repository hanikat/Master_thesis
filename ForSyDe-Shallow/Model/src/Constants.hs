--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Tue Dec 29 2020
-- Contact: hanikat@kth.se
-- Copyright: (c) 2020 Marcus Hanikat
--
-- Description: Constants used throughout model.
--------------------------------------------------
module Constants where

-- Constants
tc = 50
constantA = [1,1]
constantB = 100
constantC = 125
constantD = 50
constantE = 10
constantF = 10000
constantG = 710
constantH = 226
constantI = 0.01
constantJ = 5
constantK = 15278
constantL = 500
constantM = 139
constantO = 500
-- Verification6Limit
constantP = 300
-- verification8Limit
constantQ = 30

cyclesPerSecond = (1000 `div` tc)

-- States which each verification of the application can be within
correctState = 0
erroneousState = 1
recoveryState = 2