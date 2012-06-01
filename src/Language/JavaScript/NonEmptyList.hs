module Language.JavaScript.NonEmptyList (
 NonEmptyList,
 singleton, (<:>),
 toList

) where

-- A list of at least one element
data NonEmptyList a = Singleton a | Cons a (NonEmptyList a)

singleton :: a -> NonEmptyList a
singleton = Singleton

(<:>) :: a -> NonEmptyList a -> NonEmptyList a
x <:> xs = Cons x xs

instance Functor NonEmptyList where
  fmap f (Singleton a) = Singleton (f a)
  fmap f (Cons x xs)   = Cons (f x) (fmap f xs)

toList :: NonEmptyList a -> [a]
toList (Singleton a) = [a]
toList (Cons x xs) = x : toList xs