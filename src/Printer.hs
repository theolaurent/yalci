
module Printer where

import Ast

varList :: [ String ]
varList = fmap (\ x -> "x" ++ show x) positives
  where positives :: [ Integer ]
        positives = 1 : fmap (+1) positives

toStr :: (a -> String) -> [ String ] -> Term a -> String
toStr f _ (Var x) = f x
toStr f (h : t) (Lam u) = "(λ " ++ h ++ " · " ++ toStr (maybe h f) t u ++ ")"
toStr _ [] (Lam _) = error "ICE: this should just NOT happen" -- TODO: use infinite lists
toStr f l (App u v) = toStr f l u ++ " " ++ toStr f l v

instance Show a => Show (Term a) where
  show = toStr show varList
