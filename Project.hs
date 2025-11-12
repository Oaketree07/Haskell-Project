{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Project ( prove, proveCount ) where
import Test.QuickCheck
import Data.List
import Sequent


isAxiom :: Sequent -> Bool
isAxiom (xs :|=: ss) = any (`elem` ss) xs

lConjunction :: Sequent -> Sequent
lConjunction (((x :&&: xs) : y) :|=: (sec)) = (x : xs : y) :|=: (sec)           
lConjunction ((t : (x :&&: xs) : r) :|=: (sec)) = (t : x : xs : r) :|=: (sec)

rConjunction :: Sequent -> [Sequent]
rConjunction ((fir) :|=: ((x :&&: xs) : y)) = [(fir :|=: (x : y)) , (fir :|=: (xs : y))]
rConjunction ((fir) :|=: (t : (x :&&: xs) : y)) = [(fir :|=: (t : x : y)) , (fir :|=: (t : xs : y))]

lDisjunction :: Sequent -> [Sequent]
lDisjunction (((x :||: xs) : y) :|=: (sec)) = [((x : y) :|=: sec),((xs : y):|=: sec)]
lDisjunction (t : ((x :||: xs) : y) :|=: (sec)) = [((t : x : y) :|=: sec),((t : xs : y):|=: sec)]

rDisjunction :: Sequent -> Sequent
rDisjunction ((fir) :|=: ((x :||: xs) : y)) = (fir) :|=: (x : xs : y)
rDisjunction ((fir) :|=: (t : (x :||: xs) : y)) = (fir) :|=: (t : x : xs : y)

negation :: Sequent -> Sequent
negation (((Not x) : xs) :|=: (sec)) = (xs) :|=: (x : sec)
negation (((t : (Not x) : xs) :|=: (sec))) = (t : xs) :|=: (x : sec)
negation ((fir) :|=: ((Not x) : xs)) = (x : fir) :|=: (xs)
negation ((fir) :|=: (t : (Not x) : xs)) = (x : fir) :|=: (t : xs)

-- Proof search: returns open leaves; [] means the sequent is proved.
prove :: Sequent -> [Sequent]
prove = undefined

bigexample =
  [ (Var "a" :||: Var "f") :||: (Var "c" :||: Var "d")
  , (Var "b" :&&: Var "f") :||: (Var "f" :||: Var "a")
  , (Var "f" :||: Var "f") :&&: Not (Var "f")
  , Not (Var "a") :||: (Var "c" :||: Var "e")
  ] :|=: 
  [ Not (Var "e") :&&: Not (Var "c")
  , (Var "b" :&&: Var "c") :||: Not (Var "a")
  , Not (Var "e" :||: Var "d")
  ]

-- Examples:
example1 = [Var "c"] :|=: [ Var "a" :||: Var "b", Var "c"]
example2 = [ Var "a" :||: Var "b"] :|=: []
example3 = [] :|=: [ Var "a" :&&: Var "b"]
example4 = [ Var "a" :&&: Var "b"] :|=: []
example5 = [] :|=: [Not (Var "a")]
example6 = [Not (Var "a" :&&: Var "b")] :|=: []
-- challenge examples here

proveCount :: Sequent -> ([Sequent],Int)
proveCount = undefined
