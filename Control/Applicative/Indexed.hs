{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
This module provides a type-indexed / parameterized version of the 'Functor' and 'Applicative' classes.
-}
module Control.Applicative.Indexed where

import Control.Applicative (Applicative(pure, (<*>)))

------------------------------------------------------------------------------
-- * type-indexed / parameterized classes
------------------------------------------------------------------------------

-- | a class for a 'type-indexed' or 'paramaterized' functor
--
-- note: not sure what the most correct name is for this class, or if
-- it exists in a well supported library already.
class IndexedFunctor f where
    -- | imap is similar to fmap
    imap :: (x -> y) -- ^ function to apply to first parameter
         -> (a -> b) -- ^ function to apply to second parameter
         -> f x a    -- ^ indexed functor
         -> f y b

-- | a class for a 'type-indexed' or 'paramaterized' applicative functors
--
-- note: not sure what the most correct name is for this class, or if
-- it exists in a well supported library already.
class (IndexedFunctor f) => IndexedApplicative f where
    -- | similar to 'pure'
    ipure   :: x -> a -> f x a
    -- | similar to '<*>'
    (<<*>>) :: f (x -> y) (a -> b) -> f x a -> f y b
    -- | similar to 'Control.Applicative.*>'
    (*>>) :: f x a -> f y b -> f y b
    (*>>) = liftIA2 (const id) (const id)
    -- | similar to 'Control.Applicative.<*'
    (<<*) :: f x a -> f y b -> f x a
    (<<*) = liftIA2 const const

infixl 4 <<*>>, <<*, *>> -- , <<**>>

-- | similar to 'Data.Functor.<$>'. An alias for @imap id@
(<<$>>) :: IndexedFunctor f => (a -> b) -> f y a -> f y b
(<<$>>) = imap id

infixl 4 <<$>>

-- | A variant of '<<*>>' with the arguments reversed.
(<<**>>) :: (IndexedApplicative f) => f x a -> f (x -> y) (a -> b) -> f y b
(<<**>>) = liftIA2 (flip ($)) (flip ($))

-- | Lift a function to actions.
-- This function may be used as a value for `imap` in a `IndexedFunctor` instance.
liftIA :: (IndexedApplicative f) => (a -> b) -> (x -> y) -> f a x -> f b y
liftIA f g a = ipure f g <<*>> a

-- | Lift a binary function to actions.
liftIA2 :: (IndexedApplicative f) => (a -> b -> c) -> (x -> y -> z) -> f a x -> f b y -> f c z
liftIA2 f g a b = ipure f g <<*>> a <<*>> b

-- | Lift a binary function to actions.
liftIA3 :: (IndexedApplicative f) => (a -> b -> c -> d) -> (w -> x -> y -> z) -> f a w -> f b x -> f c y -> f d z
liftIA3 f g a b c = ipure f g <<*>> a <<*>> b <<*>> c

------------------------------------------------------------------------------
-- * WrappedApplicative
------------------------------------------------------------------------------

-- | a wrapper which lifts a value with an 'Applicative' instance so that it can be used as an 'IndexedFunctor' or 'IndexedApplicative'
--
-- > d :: WrappedApplicative Maybe y Char
-- > d = WrappedApplicative (Just succ) <<*>> WrappedApplicative (Just 'c')
newtype WrappedApplicative f index a = WrappedApplicative { unwrapApplicative :: f a }
    deriving (Functor, Applicative, Monad, Eq, Ord, Read, Show)

instance (Functor f) => IndexedFunctor (WrappedApplicative f) where
    imap f g (WrappedApplicative a) = WrappedApplicative (fmap g a)

instance (Applicative f) => IndexedApplicative (WrappedApplicative f) where
    ipure x a = WrappedApplicative (pure a)
    (WrappedApplicative f) <<*>> (WrappedApplicative a) = WrappedApplicative (f <*> a)
