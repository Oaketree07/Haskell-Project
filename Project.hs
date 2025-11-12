
module Project ( prove, proveCount ) where

import Sequent

unMaybe :: Maybe [Sequent] -> [Sequent]
unMaybe (Just xs) = xs
unMaybe Nothing = []

isAxiom :: Sequent -> Bool
isAxiom (xs :|=: ss) = any (`elem` ss) xs

lConjunction :: Sequent -> Maybe [Sequent]
lConjunction (((x :&&: xs) : ps) :|=: ss) = Just [x : xs : ps :|=: ss] 
lConjunction p = Just [p]

rConjunction :: Sequent -> Maybe [Sequent]
rConjunction (ss :|=: ((x :&&: xs) : ps)) = Just [ss :|=: x : ps, ss :|=: xs : ps]
rConjunction p = Just [p]


prove :: Sequent -> [Sequent]
prove = undefined


{-
Sequent syntax 
:|=: - |=  P entails Q where P
:||: - V   P or Q
:&&: - &&  P and Q
-}

-- Empty context entails a disjunction
example1 :: Sequent
example1 = [] :|=: [Var "A" :||: Var "B"]

-- Conjunction on the left entails one of its parts
example2 :: Sequent
example2 = [Var "A" :&&: Var "B"] :|=: [Var "A"]

-- An axiom sequent (same formula on both sides) 
example3 :: Sequent
example3 = [Var "A"] :|=: [Var "A", Var "B"]

example4 :: Sequent
example4 = [(Var "A" :&&: Var "B") :&&: Var "P"] :|=: [Var "A"]

example5 :: Sequent
example5 =[Var "A"] :|=: [(Var "A" :&&: Var "B") :&&: Var "P"]

-- for challenge part

proveCount :: Sequent -> ([Sequent],Int)
proveCount = undefined
