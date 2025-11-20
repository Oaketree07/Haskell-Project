{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Project ( prove, proveCount ) where

import Test.QuickCheck
import Data.List
import Sequent

isAxiom :: Sequent -> Bool
isAxiom (xs :|=: ss) = any (`elem` ss) xs

applyLConj :: Sequent -> Maybe Sequent
applyLConj (x :|=: y)= finder [] x  --[] is a blank list to store the checked part of the sequent
  where
    finder _ [] = Nothing  -- Whole left side has been checked 
    finder acc (f:fs) = case f of
      a :&&: b -> Just ((acc ++ [a,b] ++ fs) :|=: y)
      _        -> finder (f:acc) fs

applyRDisj :: Sequent -> Maybe Sequent
applyRDisj (x :|=: y) = finder [] y
  where
    finder _ [] = Nothing
    finder acc (f:fs) = case f of
      a :||: b -> Just (x :|=: acc ++ [a,b] ++ fs)
      _        -> finder (f:acc) fs

applyLDisj :: Sequent -> Maybe [Sequent]
applyLDisj (x :|=: y) = finder [] x
  where
    finder _ [] = Nothing
    finder acc (f:fs) = case f of
      a :||: b -> Just [(acc ++ [a] ++ fs) :|=: y , (acc ++ [b] ++ fs) :|=: y]
      _        -> finder (f:acc) fs

applyRConj :: Sequent -> Maybe [Sequent]
applyRConj (x :|=: y) = finder [] y
  where
    finder _ [] = Nothing
    finder acc (f:fs) = case f of
      a :&&: b -> Just [x :|=: (acc ++ [a] ++ fs) , x :|=: (acc ++ [b] ++ fs)]
      _        -> finder (f:acc) fs

applyNegL :: Sequent -> Maybe Sequent
applyNegL (x :|=: y) = finder [] x
  where
    finder _ [] = Nothing
    finder acc (f:fs) = case f of
      Not a -> Just ((acc ++ fs) :|=: (a : y))
      _     -> finder (f:acc) fs

applyNegR :: Sequent -> Maybe Sequent
applyNegR (x :|=: y) = finder [] y
  where
    finder _ [] = Nothing
    finder acc (f:fs) = case f of
      Not a -> Just ((a : x) :|=: (acc ++ fs))
      _     -> finder (f:acc) fs


applyRImp :: Sequent -> Maybe Sequent
applyRImp (x :|=: y) = finder [] y
  where 
    finder _ [] = Nothing
    finder acc (f:fs) = case f of
      a :->: b    -> Just ( x ++ [a] :|=: acc ++ [b] ++ fs)
      _           -> finder (f:acc) fs

applyLImp :: Sequent -> Maybe [Sequent]
applyLImp (x :|=: y) = finder [] x
  where
    finder _ [] = Nothing
    finder acc (f:fs) = case f of
      a :->: b    -> Just [(acc ++ fs :|=: [a] ++ y) , (acc ++ [b] ++ fs :|=: y) ]
      _           -> finder (f:acc) fs

applyRBImp :: Sequent -> Maybe [Sequent]
applyRBImp (x :|=: y) = finder [] y
  where
    finder _ [] = Nothing
    finder acc (f:fs) = case f of 
      a :<->: b   -> Just [(x :|=: acc ++ [a :->: b] ++ fs ) , (x :|=: acc ++ [b :->: a] ++ fs) ]
      _           -> finder (f:acc) fs

applyLBImp :: Sequent -> Maybe Sequent
applyLBImp (x :|=: y) = finder [] x
  where 
    finder _ [] = Nothing
    finder acc (f:fs) = case f of
      a :<->: b   -> Just (acc ++ [a :->: b] ++ [b :->: a] ++ fs :|=: y )
      _           -> finder (f:acc) fs


removeDuplicates :: [Sequent] -> [Sequent]
removeDuplicates s = nub [sort (nub fir) :|=: sort (nub sec) | (fir :|=: sec) <- s]
prove :: Sequent -> [Sequent]
prove s = removeDuplicates (prove' s) 
  where
    prove' s
      | isAxiom s = []
      | otherwise =
          case applyLConj s of
            Just s' -> prove' s'
            Nothing ->
              case applyRDisj s of
                Just s' -> prove' s'
                Nothing ->
                  case applyNegL s of
                    Just s' -> prove' s'
                    Nothing ->
                      case applyNegR s of
                        Just s' -> prove' s'
                        Nothing ->
                          case applyRImp s of
                            Just s' -> prove' s'
                            Nothing ->
                              case applyLBImp s of
                                Just s' -> prove s'
                                Nothing ->
                                  case applyLDisj s of
                                    Just ss -> concatMap prove' ss
                                    Nothing ->
                                      case applyRConj s of
                                        Just ss -> concatMap prove' ss
                                        Nothing ->
                                          case applyLImp s of
                                            Just ss -> concatMap prove' ss
                                            Nothing -> 
                                              case applyRBImp s of
                                                Just ss -> concatMap prove' ss
                                                Nothing ->[s]            --all the other options have been exahusted

proveCount :: Sequent -> ([Sequent],Int)
proveCount = undefined

-- exampleSequent :: Sequent
-- exampleSequent =
--   [ (Var "a" :||: Var "f") :||: (Var "c" :||: Var "d")
--   , (Var "b" :&&: Var "f") :||: (Var "f" :||: Var "a")
--   , (Var "f" :||: Var "f") :&&: Not (Var "f")
--   , Not (Var "a") :||: (Var "c" :||: Var "e")
--   ] :|=: [ Not (Var "e") :&&: Not (Var "c")
--           , (Var "b" :&&: Var "c") :||: Not (Var "a")
--           , Not (Var "e" :||: Var "d")
--           ]

-- exampleResult :: [Sequent]
-- exampleResult = prove exampleSequent

-- exampleResultCount :: ([Sequent], Int)
-- exampleResultCount = proveCount exampleSequent