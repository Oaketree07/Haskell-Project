{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Project ( prove, proveCount ) where
import Data.List
import Sequent

unMaybe :: Maybe [Sequent] -> [Sequent]
unMaybe (Just xs) = xs
unMaybe Nothing = []

isAxiom :: Sequent -> Bool
isAxiom (xs :|=: ss) = any (`elem` ss) xs

lConjunction :: Sequent -> Sequent
lConjunction (((x :&&: xs) : y) :|=: (sec)) = (x : xs : y) :|=: (sec)           
lConjunction ((t : (x :&&: xs) : r) :|=: (sec)) = (t : x : xs : r) :|=: (sec)
lConjunction p = (p)

rConjunction :: Sequent -> [Sequent]
rConjunction ((fir) :|=: ((x :&&: xs) : y)) = [(fir :|=: (x : y)) , (fir :|=: (xs : y))]
rConjunction ((fir) :|=: (t : (x :&&: xs) : y)) = [(fir :|=: (t : x : y)) , (fir :|=: (t : xs : y))]
rConjunction p = [p]

lDisjunction :: Sequent -> [Sequent]
lDisjunction (((x :||: xs) : y) :|=: (sec)) = [((x : y) :|=: sec),((xs : y):|=: sec)]
lDisjunction (t : ((x :||: xs) : y) :|=: (sec)) = [((t : x : y) :|=: sec),((t : xs : y):|=: sec)]
lDisjunction p = [p]

rDisjunction :: Sequent -> Sequent
rDisjunction ((fir) :|=: ((x :||: xs) : y)) = (fir) :|=: (x : xs : y)
rDisjunction ((fir) :|=: (t : (x :||: xs) : y)) = (fir) :|=: (t : x : xs : y)
rDisjunction p = p

negation :: Sequent -> Sequent
negation (((Not x) : xs) :|=: (sec)) = (xs) :|=: (x : sec)
negation (((t : (Not x) : xs) :|=: (sec))) = (t : xs) :|=: (x : sec)
negation ((fir) :|=: ((Not x) : xs)) = (x : fir) :|=: (xs)
negation ((fir) :|=: (t : (Not x) : xs)) = (x : fir) :|=: (t : xs)
negation p = p

prove :: Sequent -> [Sequent]
prove = undefined


-- Examples:
example1 = [] :|=: [ Var "a" :||: Var "b"]
example2 = [ Var "a" :||: Var "b"] :|=: []
example3 = [] :|=: [ Var "a" :&&: Var "b"]
example4 = [ Var "a" :&&: Var "b"] :|=: []
example5 = [] :|=: [Not (Var "a")]
example6 = [Not (Var "a" :&&: Var "b")] :|=: []
-- challenge examples here

proveCount :: Sequent -> ([Sequent],Int)
proveCount = undefined
