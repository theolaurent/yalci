
module Krivine where

import Ast

type State a = (Term a, [ Term a ])
type EvCst a = a -> [ Term a ] -> Maybe (State a)

step :: EvCst a -> State a -> Maybe (State a)
step f (App u v, s    ) = Just (u, v : s)
step f (Lam t  , u : r) = Just (substUnderLambda u t, r)
step f (Lam _  , []   ) = Nothing
step f (Var x  , s    ) = f x s


eval :: EvCst a -> Term a -> Term a
eval f x = loop (x, []) where -- TODO: monadic operator
  loop (e, c) = case step f (e, c) of
    Just a -> loop a
    Nothing -> e

states :: EvCst a -> Term a -> [ Term a ]
states f x = x : loop (x, []) where
  loop c = case step f c of
    Nothing -> []
    Just (e, r) -> e : loop (e, r)
