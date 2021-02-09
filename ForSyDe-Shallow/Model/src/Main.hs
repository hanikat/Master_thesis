--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Tue Dec 25 2020
-- Contact: hanikat@kth.se
-- Copyright: (c) 2020 Marcus Hanikat
--
-- Description: 
--------------------------------------------------

module Main where

import ForSyDe.Shallow

import Requirement1
import Requirement2
import Requirement3
import Requirement4
import Requirement5
import Requirement6
import Requirement7
import Requirement8
import Requirement9
import Requirement10
import Requirement11
import Requirement12
import Requirement14
import Requirement15
import Requirement20
import Requirement21
import Requirement23
import Requirement24
import Requirement25
import Requirement26

import Verification1
import Verification2
import Verification3
import Verification4
import Verification5
import Verification6
import Verification8

import Constants

-- Signals
-- (signalA[0][0],(signalA[0][1],(signalA[1][0],(signalA[1][1])
signalA = signal [(-10001,-10001,9990,10000),(-10001,-10001,9990,10000),(1,1,1,1),(1,1,1,1),(-10001,-10001,9990,10000),(-10001,-10001,9990,10000)]
-- (signalB[0],signalB[1])
signalB = signal [(1000,10),(500,100),(500,100),(500,100),(1000,100),(1000,100),(500,1000),(500,1000)]
-- (signalC[0][0],signalC[0][1],signalC[1][0],signalC[1][1])
signalC = signal [(10,200,25,500),(10,50,300,50),(110,150,1000,10),(200,10,0,10),(80,75,95,100),(30,130,230,240),(50,50,160,25),(0,0,60,500)]



-- Process constructors for requirements modeled
req1SDF :: Signal (Int, Int, Int, Int) -> Signal (Int, Int, Int, Int)
req1SDF = actor11SDF 1 1 requirement1;

req2SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> (Signal (Int,Int,Int,Int), Signal (Int,Int))
req2SDF signalAInput signalBInput signalCInput = actor22SDF (1,1) (1,1) requirement2 (req1SDF signalAInput) (delaySDF [(0,0),(0,0)] $ req21SDF signalAInput signalBInput signalCInput);

req3SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int, Int, Int, Int)
req3SDF signalAInput signalBInput signalCInput = actor11SDF 1 1 requirement3 . fst $ req2SDF signalAInput signalBInput signalCInput;

req4SDFDelayer :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int,Int,Int)
req4SDFDelayer signalAInput signalBInput signalCInput = outputE
    where {
         (outputE,outputF) = req4SDF (outputEdelayed,outputFdelayed) (fst $ req2SDF signalAInput signalBInput signalCInput) (req3SDF signalAInput signalBInput signalCInput);
         outputEdelayed = delaySDF [(0,0,0,0)] outputE;
         outputFdelayed = delaySDF [(0,0,0,0)] outputF;
    }

req4SDF :: (Signal (Int,Int,Int,Int), Signal (Int,Int,Int,Int)) -> Signal (Int, Int, Int, Int) -> Signal (Int, Int, Int, Int) -> (Signal (Int,Int,Int,Int), Signal (Int,Int,Int,Int))
req4SDF (outputEdelayed,outputFdelayed) outputB outputD = (outputE,outputF)
     where {
          (outputE,outputF) = actor42SDF (1,1,1,1) (1,1) requirement4 outputEdelayed outputFdelayed outputB outputD;
     }

req5SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int, Int, Int, Int)
req5SDF signalAInput signalBInput signalCInput = actor31SDF (1,1,1) 1 requirement5 (fst $ req2SDF signalAInput signalBInput signalCInput) signalBInput signalCInput

req6SDFDelayer :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int,Int,Int)
req6SDFDelayer signalAInput signalBInput signalCInput = outputH
    where {
         (outputH,outputI) = req6SDF (outputHdelayed,outputIdelayed) (fst $ req2SDF signalAInput signalBInput signalCInput) (req5SDF signalAInput signalBInput signalCInput);
         outputHdelayed = delaySDF [(0,0,0,0)] outputH;
         outputIdelayed = delaySDF [(0,0,0,0)] outputI;
    }

req6SDF :: (Signal (Int,Int,Int,Int), Signal (Int,Int,Int,Int)) -> Signal (Int, Int, Int, Int) -> Signal (Int, Int, Int, Int) -> (Signal (Int,Int,Int,Int), Signal (Int,Int,Int,Int))
req6SDF (outputHdelayed,outputIdelayed) outputB outputG = (outputH,outputI)
     where {
          (outputH,outputI) = actor42SDF (1,1,1,1) (1,1) requirement6 outputHdelayed outputIdelayed outputB outputG;
     }

req7SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int,Int,Int)
req7SDF signalAInput signalBInput signalCInput = actor31SDF (1,1,1) 1 requirement7 (fst $ req2SDF signalAInput signalBInput signalCInput) signalBInput signalCInput

req8SDFDelayer :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int,Int,Int)
req8SDFDelayer signalAInput signalBInput signalCInput = outputK
    where {
         (outputK,outputL) = req8SDF (outputKdelayed,outputLdelayed) (fst $ req2SDF signalAInput signalBInput signalCInput) (req7SDF signalAInput signalBInput signalCInput);
         outputKdelayed = delaySDF [(0,0,0,0)] outputK;
         outputLdelayed = delaySDF [(0,0,0,0)] outputL;
    }

req8SDF :: (Signal (Int,Int,Int,Int), Signal (Int,Int,Int,Int)) -> Signal (Int, Int, Int, Int) -> Signal (Int, Int, Int, Int) -> (Signal (Int,Int,Int,Int), Signal (Int,Int,Int,Int))
req8SDF (outputKdelayed,outputLdelayed) outputB outputG = (outputK,outputL)
     where {
          (outputK,outputL) = actor42SDF (1,1,1,1) (1,1) requirement8 outputKdelayed outputLdelayed outputB outputG;
     }

req9SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int)
req9SDF signalAInput signalBInput signalCInput = actor11SDF 1 1 requirement9 (req4SDFDelayer signalAInput signalBInput signalCInput);

req10SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int)
req10SDF signalAInput signalBInput signalCInput = actor11SDF 1 1 requirement10 (req6SDFDelayer signalAInput signalBInput signalCInput);

req11SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int)
req11SDF signalAInput signalBInput signalCInput = actor11SDF 1 1 requirement11 (req8SDFDelayer signalAInput signalBInput signalCInput);

req12SDFDelayer :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int) 
req12SDFDelayer signalAInput signalBInput signalCInput = outputP
     where {
          (outputP,outputQ) = req12SDF (outputPdelayed,outputQdelayed) (req9SDF signalAInput signalBInput signalCInput);
          outputPdelayed = delaySDF [(0,0)] outputP;
          outputQdelayed = delaySDF [(0,0)] outputQ;
     }

req12SDF :: (Signal (Int,Int), Signal (Int,Int)) -> Signal (Int, Int) -> (Signal (Int,Int), Signal (Int,Int))
req12SDF (outputRdelayed,outputSdelayed) outputN = (outputR,outputS)
     where {
          (outputR,outputS) = actor32SDF (1,1,1) (1,1) requirement12 outputRdelayed outputSdelayed outputN;
     }

req14SDFDelayer :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int)
req14SDFDelayer signalAInput signalBInput signalCInput = outputR
     where {
          (outputR,outputS) = req14SDF (outputRdelayed,outputSdelayed) (req10SDF signalAInput signalBInput signalCInput);
          outputRdelayed = delaySDF [(0,0)] outputR;
          outputSdelayed = delaySDF [(0,0)] outputS;
     }

req14SDF :: (Signal (Int,Int), Signal (Int,Int)) -> Signal (Int, Int) -> (Signal (Int,Int), Signal (Int,Int))
req14SDF (outputRdelayed,outputSdelayed) outputN = (outputR,outputS)
     where {
          (outputR,outputS) = actor32SDF (1,1,1) (1,1) requirement14 outputRdelayed outputSdelayed outputN;
     }

req15SDFDelayer :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int)
req15SDFDelayer signalAInput signalBInput signalCInput = outputT
     where {
          (outputT,outputU) = req15SDF (outputTdelayed,outputUdelayed) (req11SDF signalAInput signalBInput signalCInput);
          outputTdelayed = delaySDF [(0,0)] outputT;
          outputUdelayed = delaySDF [(0,0)] outputU;
     }

req15SDF :: (Signal (Int,Int), Signal (Int,Int)) -> Signal (Int, Int) -> (Signal (Int,Int), Signal (Int,Int))
req15SDF (outputTdelayed,outputUdelayed) outputO = (outputT,outputU)
     where {
          (outputT,outputU) = actor32SDF (1,1,1) (1,1) requirement15 outputTdelayed outputUdelayed outputO;
     }

req20SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int, Int)
req20SDF signalAInput signalBInput signalCInput = actor21SDF (1,1) 1 requirement20 (snd $ req2SDF signalAInput signalBInput signalCInput) (req12SDFDelayer signalAInput signalBInput signalCInput);

req21SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int, Int)
req21SDF signalAInput signalBInput signalCInput = actor31SDF (1,1,1) 1 requirement21 (req14SDFDelayer signalAInput signalBInput signalCInput) (req15SDFDelayer signalAInput signalBInput signalCInput) (req20SDF signalAInput signalBInput signalCInput);

req23SDFDelayer :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int,Int,Int)
req23SDFDelayer signalAInput signalBInput signalCInput = outputX
     where {
          outputX = req23SDF outputEdelayed outputE;
          outputEdelayed = delaySDF [(0,0,0,0),(0,0,0,0)] outputE;
          outputE = req4SDFDelayer signalAInput signalBInput signalCInput;
     }

req23SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int,Int,Int)
req23SDF outputEdelayed outputE = outputX
     where {
          outputX = actor21SDF (1,1) 1 requirement23 outputEdelayed outputE;
     } 

req24SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int, Int)
req24SDF signalAInput signalBInput signalCInput = actor11SDF 1 1 requirement24 (req23SDFDelayer signalAInput signalBInput signalCInput);

req25SDFDelayer :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int)
req25SDFDelayer signalAInput signalBInput signalCInput = outputZ
     where {
          (outputZ,outputAA) = req25SDF (req2SDF signalAInput signalBInput signalCInput) outputAAdelayed;
          outputAAdelayed = delaySDF [(0,0)] outputAA;
     }

req25SDF :: (Signal (Int,Int,Int,Int), Signal (Int,Int)) -> Signal (Int,Int) -> (Signal (Int,Int), Signal (Int,Int))
req25SDF (outputB,outputC) outputAAdelayed = (outputZ,outputAA)
     where {
          (outputZ,outputAA) = actor32SDF (1,1,1) (1,1) requirement25 outputB outputC outputAAdelayed;
     }

req26SDFDelayer :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int)
req26SDFDelayer signalAInput signalBInput signalCInput = outputAB
     where {
          outputAB = req26SDF (req25SDFDelayer signalAInput signalBInput signalCInput) outputABdelayed;
          outputABdelayed = delaySDF [(0,0)] outputAB;
     }

req26SDF :: Signal (Int,Int) -> Signal (Int,Int) -> Signal (Int,Int)
req26SDF outputAA outputABdelayed = outputAB
     where {
          outputAB = actor21SDF (1,1) 1 requirement26 outputAA outputABdelayed;
     } 


verification1SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int)
verification1SDF signalAInput signalBInput signalCInput = states
     where {
          verificaiton1Output = actor42SDF (1,1,1,1) (1,1) verification1 (snd (req2SDF signalAInput signalBInput signalCInput)) (delaySDF [(0,0),(0,0)] $ req21SDF signalAInput signalBInput signalCInput) states counters;
          states = delaySDF [(0,0)] $ fst verificaiton1Output;
          counters = delaySDF [(0,0)] $ snd verificaiton1Output;
     }

verification2SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int)
verification2SDF signalAInput signalBInput signalCInput = states
     where {
          verificaiton2Output = actor22SDF (1,1) (1,1) verification2 (fst (req2SDF signalAInput signalBInput signalCInput)) differenceSum;
          states = fst verificaiton2Output;
          differenceSum = delaySDF [(0,0)] $ snd verificaiton2Output;
     }

verification3SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int)
verification3SDF signalAInput signalBInput signalCInput = actor11SDF 1 1 verification3 (req24SDF signalAInput signalBInput signalCInput);

verification4SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int)
verification4SDF signalAInput signalBInput signalCInput = actor11SDF 1 1 verification4 (req12SDFDelayer signalAInput signalBInput signalCInput);

verification5SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int)
verification5SDF signalAInput signalBInput signalCInput = states
     where {
          cycleLimit = toInteger (30 * (1000 `div` fromIntegral tc));
          (states,oldSignal,counter) = actor33SDF (1,1,1) (1,1,1) verification5 (req12SDFDelayer signalAInput signalBInput signalCInput) delayedOldSignal delayedCounter;
          delayedOldSignal = delaySDF (take (fromInteger cycleLimit) (repeat (0,0))) $ oldSignal;
          delayedCounter = delaySDF [0] counter;
     }


verification6SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int)
verification6SDF signalAInput signalBInput signalCInput = actor11SDF 1 1 verification6 (verification5SDF signalAInput signalBInput signalCInput);

verification8SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int)
verification8SDF signalAInput signalBInput signalCInput = states
     where {
          (states,counter) = actor22SDF (1,1) (1,1) verification8 (req26SDFDelayer signalAInput signalBInput signalCInput) delayedCounter;
          delayedCounter = delaySDF [0] counter;
     }

-- Function to run different parts of the model
runModel1 :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int)
runModel1 signalAInput signalBInput signalCInput = req21SDF signalAInput signalBInput signalCInput

runModel2 :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int, Int)
runModel2 signalAInput signalBInput signalCInput = req14SDFDelayer signalAInput signalBInput signalCInput

runModel3 :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int)
runModel3 signalAInput signalBInput signalCInput = req15SDFDelayer signalAInput signalBInput signalCInput

runModel4 :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int)
runModel4 signalAInput signalBInput signalCInput = req24SDF signalAInput signalBInput signalCInput

runModel5 :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int)
runModel5 signalAInput signalBInput signalCInput = req26SDFDelayer signalAInput signalBInput signalCInput

runModel6 :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int)
runModel6 signalAInput signalBInput signalCInput = verification1SDF signalAInput signalBInput signalCInput

runModel7 :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int)
runModel7 signalAInput signalBInput signalCInput = verification2SDF signalAInput signalBInput signalCInput

runModel8 :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int)
runModel8 signalAInput signalBInput signalCInput = verification3SDF signalAInput signalBInput signalCInput

runModel9 :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int)
runModel9 signalAInput signalBInput signalCInput = verification4SDF signalAInput signalBInput signalCInput

runModel10 :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int)
runModel10 signalAInput signalBInput signalCInput = verification5SDF signalAInput signalBInput signalCInput

runModel11 :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int)
runModel11 signalAInput signalBInput signalCInput = verification6SDF signalAInput signalBInput signalCInput

runModel12 :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int)
runModel12 signalAInput signalBInput signalCInput = verification8SDF signalAInput signalBInput signalCInput

-- Print results from running different sub-sets of the model
main = do
    print $ runModel1 signalA signalB signalC
    print $ runModel2 signalA signalB signalC
    print $ runModel3 signalA signalB signalC
    print $ runModel4 signalA signalB signalC
    print $ runModel5 signalA signalB signalC
    print $ runModel6 signalA signalB signalC
    print $ runModel7 signalA signalB signalC
    print $ runModel8 signalA signalB signalC
    print $ runModel9 signalA signalB signalC
    print $ runModel10 signalA signalB signalC
    print $ runModel11 signalA signalB signalC
    print $ runModel12 signalA signalB signalC
