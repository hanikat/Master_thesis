--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Tue Dec 25 2020
-- Contact: hanikat@kth.se
-- Copyright: (c) 2020 Marcus Hanikat
--
-- Description: Contains the construction of ForSyDe processes, 
-- signal defintions and main method for model execution
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
signalA = signal [(1,0,0,0),(0,0,1,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0)]
-- (signalB[0],signalB[1])
signalB = signal [(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0)]
-- (signalC[0][0],signalC[0][1],signalC[1][0],signalC[1][1])
signalC = signal [(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0)]

outputW = [(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0)]

-- Process constructors for requirements modeled
req1SDF :: Signal (Int, Int, Int, Int) -> Signal (Int, Int, Int, Int)
req1SDF = actor11SDF 1 1 requirement1;

req2SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> (Signal (Int,Int,Int,Int), Signal (Int,Int))
req2SDF signalAInput signalBInput signalCInput = actor22SDF (1,1) (1,1) requirement2 (req1SDF signalAInput) (delaySDF outputW $ req21SDF signalAInput signalBInput signalCInput);

req3SDF :: (Fractional a, Ord a) => Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (a, a, a, a)
req3SDF signalAInput signalBInput signalCInput = actor11SDF 1 1 requirement3 . fst $ req2SDF signalAInput signalBInput signalCInput;

req4SDFDelayer :: (Fractional a, Ord a) => Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (a,a,a,a)
req4SDFDelayer signalAInput signalBInput signalCInput = outputE
    where {
         (outputE,outputF) = req4SDF (outputEdelayed,outputFdelayed) (fst $ req2SDF signalAInput signalBInput signalCInput) (req3SDF signalAInput signalBInput signalCInput);
         outputEdelayed = delaySDF [(0,0,0,0)] outputE;
         outputFdelayed = delaySDF [(0,0,0,0)] outputF;
    }

req4SDF :: (Fractional a, Ord a) => (Signal (a,a,a,a), Signal (a,a,a,a)) -> Signal (Int, Int, Int, Int) -> Signal (a, a, a, a) -> (Signal (a,a,a,a), Signal (a,a,a,a))
req4SDF (outputEdelayed,outputFdelayed) outputB outputD = (outputE,outputF)
     where {
          (outputE,outputF) = actor42SDF (1,1,1,1) (1,1) requirement4 outputEdelayed outputFdelayed outputB outputD;
     }

req5SDF :: (Fractional a, Ord a) => Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (a,a,a,a)
req5SDF signalAInput signalBInput signalCInput = actor31SDF (1,1,1) 1 requirement5 (fst $ req2SDF signalAInput signalBInput signalCInput) signalBInput signalCInput

req6SDFDelayer :: (Fractional a, Ord a) => Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (a,a,a,a)
req6SDFDelayer signalAInput signalBInput signalCInput = outputH
    where {
         (outputH,outputI) = req6SDF (outputHdelayed,outputIdelayed) (fst $ req2SDF signalAInput signalBInput signalCInput) (req5SDF signalAInput signalBInput signalCInput);
         outputHdelayed = delaySDF [(0,0,0,0)] outputH;
         outputIdelayed = delaySDF [(0,0,0,0)] outputI;
    }

req6SDF :: (Fractional a, Ord a) => (Signal (a,a,a,a), Signal (a,a,a,a)) -> Signal (Int, Int, Int, Int) -> Signal (a,a,a,a) -> (Signal (a,a,a,a), Signal (a,a,a,a))
req6SDF (outputHdelayed,outputIdelayed) outputB outputG = (outputH,outputI)
     where {
          (outputH,outputI) = actor42SDF (1,1,1,1) (1,1) requirement6 outputHdelayed outputIdelayed outputB outputG;
     }

req7SDF :: (Fractional a, Ord a) => Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (a,a,a,a)
req7SDF signalAInput signalBInput signalCInput = actor31SDF (1,1,1) 1 requirement7 (fst $ req2SDF signalAInput signalBInput signalCInput) signalBInput signalCInput

req8SDFDelayer :: (Fractional a, Ord a) => Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (a,a,a,a)
req8SDFDelayer signalAInput signalBInput signalCInput = outputK
    where {
         (outputK,outputL) = req8SDF (outputKdelayed,outputLdelayed) (fst $ req2SDF signalAInput signalBInput signalCInput) (req7SDF signalAInput signalBInput signalCInput);
         outputKdelayed = delaySDF [(0,0,0,0)] outputK;
         outputLdelayed = delaySDF [(0,0,0,0)] outputL;
    }

req8SDF :: (Fractional a, Ord a) => (Signal (a,a,a,a), Signal (a,a,a,a)) -> Signal (Int, Int, Int, Int) -> Signal (a, a, a, a) -> (Signal (a,a,a,a), Signal (a,a,a,a))
req8SDF (outputKdelayed,outputLdelayed) outputB outputG = (outputK,outputL)
     where {
          (outputK,outputL) = actor42SDF (1,1,1,1) (1,1) requirement8 outputKdelayed outputLdelayed outputB outputG;
     }

req9SDF :: (Fractional a, Ord a) => Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (a,a)
req9SDF signalAInput signalBInput signalCInput = actor11SDF 1 1 requirement9 (req4SDFDelayer signalAInput signalBInput signalCInput);

req10SDF :: (Fractional a, Ord a) => Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (a,a)
req10SDF signalAInput signalBInput signalCInput = actor11SDF 1 1 requirement10 (req6SDFDelayer signalAInput signalBInput signalCInput);

req11SDF :: (Fractional a, Ord a) => Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (a,a)
req11SDF signalAInput signalBInput signalCInput = actor11SDF 1 1 requirement11 (req8SDFDelayer signalAInput signalBInput signalCInput);

req12SDFDelayer :: (Fractional a, Ord a) => Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (a,a) 
req12SDFDelayer signalAInput signalBInput signalCInput = outputP
     where {
          (outputP,outputQ) = req12SDF (outputPdelayed,outputQdelayed) (req9SDF signalAInput signalBInput signalCInput);
          outputPdelayed = delaySDF [(0,0)] outputP;
          outputQdelayed = delaySDF [(0,0)] outputQ;
     }

req12SDF :: (Fractional a, Ord a) => (Signal (a,a), Signal (a,a)) -> Signal (a, a) -> (Signal (a,a), Signal (a,a))
req12SDF (outputRdelayed,outputSdelayed) outputM = (outputR,outputS)
     where {
          (outputR,outputS) = actor32SDF (1,1,1) (1,1) requirement12 outputRdelayed outputSdelayed outputM;
     }

req14SDFDelayer :: (Fractional a, Ord a) => Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (a, a)
req14SDFDelayer signalAInput signalBInput signalCInput = outputR
     where {
          (outputR,outputS) = req14SDF (outputRdelayed,outputSdelayed) (req10SDF signalAInput signalBInput signalCInput);
          outputRdelayed = delaySDF [(0,0)] outputR;
          outputSdelayed = delaySDF [(0,0)] outputS;
     }

req14SDF :: (Fractional a, Ord a) => (Signal (a,a), Signal (a,a)) -> Signal (a, a) -> (Signal (a,a), Signal (a,a))
req14SDF (outputRdelayed,outputSdelayed) outputN = (outputR,outputS)
     where {
          (outputR,outputS) = actor32SDF (1,1,1) (1,1) requirement14 outputRdelayed outputSdelayed outputN;
     }

req15SDFDelayer :: (Fractional a, Ord a) => Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (a, a)
req15SDFDelayer signalAInput signalBInput signalCInput = outputT
     where {
          (outputT,outputU) = req15SDF (outputTdelayed,outputUdelayed) (req11SDF signalAInput signalBInput signalCInput);
          outputTdelayed = delaySDF [(0,0)] outputT;
          outputUdelayed = delaySDF [(0,0)] outputU;
     }

req15SDF :: (Fractional a, Ord a) => (Signal (a, a), Signal (a, a)) -> Signal (a, a) -> (Signal (a, a), Signal (a, a))
req15SDF (outputTdelayed,outputUdelayed) outputO = (outputT,outputU)
     where {
          (outputT,outputU) = actor32SDF (1,1,1) (1,1) requirement15 outputTdelayed outputUdelayed outputO;
     }

req20SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int, Int)
req20SDF signalAInput signalBInput signalCInput = actor21SDF (1,1) 1 requirement20 (snd $ req2SDF signalAInput signalBInput signalCInput) (req12SDFDelayer signalAInput signalBInput signalCInput);

req21SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int, Int)
req21SDF signalAInput signalBInput signalCInput = actor31SDF (1,1,1) 1 requirement21 (req14SDFDelayer signalAInput signalBInput signalCInput) (req15SDFDelayer signalAInput signalBInput signalCInput) (req20SDF signalAInput signalBInput signalCInput);

req23SDFDelayer :: (Fractional a, Ord a) => Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (a,a,a,a)
req23SDFDelayer signalAInput signalBInput signalCInput = outputX
     where {
          outputX = req23SDF outputEdelayed outputE;
          outputEdelayed = delaySDF [(0,0,0,0),(0,0,0,0)] outputE;
          outputE = req4SDFDelayer signalAInput signalBInput signalCInput;
     }

req23SDF :: (Fractional a, Ord a) => Signal (a,a,a,a) -> Signal (a,a,a,a) -> Signal (a,a,a,a)
req23SDF outputEdelayed outputE = outputX
     where {
          outputX = actor21SDF (1,1) 1 requirement23 outputEdelayed outputE;
     }

req24SDF :: (Fractional a, Ord a) => Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (a, a)
req24SDF signalAInput signalBInput signalCInput = actor11SDF 1 1 requirement24 (req23SDFDelayer signalAInput signalBInput signalCInput);

req25SDFDelayer :: (Fractional a, Ord a) => Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (a,a)
req25SDFDelayer signalAInput signalBInput signalCInput = outputZ
     where {
          (outputZ,outputAA) = req25SDF (req2SDF signalAInput signalBInput signalCInput) outputAAdelayed;
          outputAAdelayed = delaySDF [(0,0)] outputAA;
     }

req25SDF :: (Fractional a, Ord a) => (Signal (Int,Int,Int,Int), Signal (Int,Int)) -> Signal (a,a) -> (Signal (a,a), Signal (a,a))
req25SDF (outputB,outputC) outputAAdelayed = (outputZ,outputAA)
     where {
          (outputZ,outputAA) = actor32SDF (1,1,1) (1,1) requirement25 outputB outputC outputAAdelayed;
     }

req26SDFDelayer :: (Fractional a, Ord a) => Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (a,a)
req26SDFDelayer signalAInput signalBInput signalCInput = outputAB
     where {
          outputAB = req26SDF (req25SDFDelayer signalAInput signalBInput signalCInput) outputABdelayed;
          outputABdelayed = delaySDF [(0,0)] outputAB;
     }

req26SDF :: (Fractional a, Ord a) => Signal (a,a) -> Signal (a,a) -> Signal (a,a)
req26SDF outputAA outputABdelayed = outputAB
     where {
          outputAB = actor21SDF (1,1) 1 requirement26 outputAA outputABdelayed;
     } 


verification1SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int)
verification1SDF signalAInput signalBInput signalCInput = states
     where {
          (states, counters) = actor42SDF (1,1,1,1) (1,1) verification1 (snd (req2SDF signalAInput signalBInput signalCInput)) (delaySDF [(0,0),(0,0)] $ req21SDF signalAInput signalBInput signalCInput) delayedStates delayedCounters;
          delayedStates = delaySDF [(0,0)] $ states;
          delayedCounters = delaySDF [(0,0)] $ counters;
     }

verification2SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int)
verification2SDF signalAInput signalBInput signalCInput = states
     where {
          (states, differenceSums, counters) = actor33SDF (1,1,1) (1,1,1) verification2 (fst (req2SDF signalAInput signalBInput signalCInput)) delayedDifferenceSum delayedCouters;
          delayedDifferenceSum = delaySDF [(0,0)] $ differenceSums;
          delayedCouters = delaySDF [(0,0)] $ counters;
     }

verification3SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int)
verification3SDF signalAInput signalBInput signalCInput = states
     where {
          (states,counters) = actor22SDF (1,1) (1,1) verification3 (req24SDF signalAInput signalBInput signalCInput) delayedCounters;
          delayedCounters = delaySDF [0] $ counters;
     }
     

verification4SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int)
verification4SDF signalAInput signalBInput signalCInput = states
     where {
          (states, counters) = actor22SDF (1,1) (1,1) verification4 (req12SDFDelayer signalAInput signalBInput signalCInput) delayedCounters;
          delayedCounters = delaySDF [(0,0)] $ counters;
     }
     

verification5SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int)
verification5SDF signalAInput signalBInput signalCInput = states
     where {
          cycleLimit = toInteger (30 * cyclesPerSecond);
          (states,oldSignal,counter,recoveryCounter) = actor44SDF (1,1,1,1) (1,1,1,1) verification5 (req12SDFDelayer signalAInput signalBInput signalCInput) delayedOldSignal delayedCounter delayedRecoveryCounter;
          delayedOldSignal = delaySDF (take (fromInteger cycleLimit) (repeat (0,0))) $ oldSignal;
          delayedCounter = delaySDF [0] counter;
          delayedRecoveryCounter = delaySDF [0] recoveryCounter;
     }


verification6SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int)
verification6SDF signalAInput signalBInput signalCInput = actor11SDF 1 1 verification6 (verification5SDF signalAInput signalBInput signalCInput);

verification8SDF :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int)
verification8SDF signalAInput signalBInput signalCInput = states
     where {
          (states,counter) = actor22SDF (1,1) (1,1) verification8 (req26SDFDelayer signalAInput signalBInput signalCInput) delayedCounter;
          delayedCounter = delaySDF [0] counter;
     }

-- Function to setup SDF graph
runModel :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int)
runModel signalAInput signalBInput signalCInput = verification3SDF signalAInput signalBInput signalCInput

runModel1 :: Signal (Int,Int,Int,Int) -> Signal (Int,Int) -> Signal (Int,Int,Int,Int) -> Signal (Int,Int)
runModel1 signalAInput signalBInput signalCInput = req21SDF signalAInput signalBInput signalCInput

main = do
    print $ runModel signalA signalB signalC
    print $ runModel1 signalA signalB signalC

     
