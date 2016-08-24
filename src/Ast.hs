{-# LANGUAGE DeriveFunctor, DeriveTraversable #-}

module Ast where

import Control.Monad (ap)

-- The type parameter a represent all and only free variables.
data Term a = Var a
            | Lam (Term (Maybe a))
            | App (Term a) (Term a)
            deriving (Eq, Functor, Foldable, Traversable)

instance Monad Term where
  return = Var
  a >>= f = join (fmap f a)
    where join :: Term (Term a) -> Term a
          join (Var t) = t
          join (Lam t) = Lam (join $ fmap sequence t) -- humm (join (fmap ...)) is bind
          join (App x y) = App (join x) (join y)

instance Applicative Term where
  pure = return
  (<*>) = ap

lambda :: Eq a => a -> Term a -> Term a
lambda x t = Lam $ fmap (\ y -> if x == y then Nothing else Just y) t

substOne :: Eq a => Term a -> a -> Term a -> Term a
substOne u v t = (\ x -> if x == v then u else Var x) =<< t

substUnderLambda :: Term a -> Term (Maybe a) -> Term a
substUnderLambda u t = maybe u Var =<< t -- I <3 Haskell

isClosed :: Term a -> Maybe (Term b)
isClosed = sequence . fmap (const Nothing)

-- test :: Maybe (Term b)
-- test = isClosed $ lambda "42" $ Lam $ Lam $ (App (Var Nothing) (App (Var (Just Nothing)) (Var (Just (Just "42")))))
