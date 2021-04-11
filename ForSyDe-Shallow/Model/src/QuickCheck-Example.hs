import ForSyDe.Shallow
import Test.QuickCheck

formalSum :: Fractional a => [a] -> [a] -> [a]
formalSum addendsA addendsB = zipWith (+) addendsA addendsB

formalDivision :: Fractional a => [a] -> [a] -> [a]
formalDivision dividends divisors = zipWith (/) dividends divisors

sumSDF :: Fractional a => Signal a -> Signal a -> Signal a
sumSDF = actor21SDF (1,1) 1 formalSum

integerDivisionSDF :: Fractional a =>  Signal a -> Signal a -> Signal a
integerDivisionSDF = actor21SDF (1,1) 1 formalIntegerDivision

averageSDF :: Fractional a =>  Signal a -> Signal a -> Signal a
averageSDF signalA signalB = actor21SDF (1,1) 1 (integerDivisionSDF (sumSDF signalA signalB) (signal [2..]))

property_less_or_eq_to_max :: (Fractional a, Ord a) => [a] -> [a] -> Property
property_less_or_eq_to_max sigA sigB = (length sigA == 1 && length sigB == 1) ==> 
   headS (averageSDF (signal sigA) (signal sigB)) <= max (head sigA) (head sigB)
