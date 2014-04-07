-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeOperators #-}

module Data.Predicate
    ( -- * Predicate
      Predicate
    , constant
    , failure
    , true
    , false
    , and
    , or
    , orElse
    , (.&.)
    , (.|.)
    , (|||)
    , opt
    , def
    , mapOkay
    , mapFail
    , exec
    -- * Result
    , module Data.Predicate.Result
    -- * Product
    , module Data.Predicate.Product
    ) where

import Control.Monad
import Data.Predicate.Product
import Data.Predicate.Result
import Prelude hiding (and, or)

-- | A predicate is a function of some value of type @a@ to a 'Result',
-- i.e. a 'Bool'-like value with 'Okay' as 'True' and 'Fail' as 'False',
-- which carries additional data in each branch.
type Predicate a f t = a -> Result f t

-- | A predicate which always returns @Okay@ with the given
-- value as metadata.
constant :: t -> Predicate a f t
constant t _ = return t

true :: Predicate a f ()
true = constant ()

-- | A predicate which always returns @Fail@ with the given
-- value as metadata.
failure :: f -> Predicate a f t
failure f _ = Fail f

false :: Predicate a () t
false = failure ()

infixr 3 .&.
infixr 2 .|.
infixr 2 |||

-- | A predicate corresponding to the logical AND connective
-- of two predicate.
and :: Predicate a f t -> Predicate a f t' -> Predicate a f (t ::: t')
and f g x = f x `cmp` g x
  where
    cmp (Okay d y) (Okay w z) = Okay (d + w) (y ::: z)
    cmp (Okay _ _) (Fail   y) = Fail y
    cmp (Fail   y) _          = Fail y

-- | A predicate corresponding to the logical
-- OR connective of two predicates. It requires the
-- metadata of each @Okay@ branch to be of the same type.
--
-- If both arguments evaluate to @Okay@ the one with the
-- smaller \"delta\" will be preferred, or--if equal--the
-- left-hand argument.
or :: Predicate a f t -> Predicate a f t -> Predicate a f t
or f g x = f x `cmp` g x
  where
    cmp a@(Okay d _) b@(Okay w _)  = if w < d then b else a
    cmp a@(Okay _ _)   (Fail _)    = a
    cmp (Fail _)     b@(Okay _ _)  = b
    cmp (Fail _)     b@(Fail _)    = b

-- | A predicate corresponding to the logical
-- OR connective of two predicates. The metadata of
-- each @Okay@ branch can be of different types.
--
-- If both arguments evaluate to @Okay@ the one with the
-- smaller \"delta\" will be preferred, or--if equal--the
-- left-hand argument.
orElse :: Predicate a f t -> Predicate a f t' -> Predicate a f (Either t t')
orElse f g x = f x `cmp` g x
  where
    cmp (Okay d y) (Okay w z) = if w < d then Okay w (Right z) else Okay d (Left y)
    cmp (Okay d y) (Fail   _) = Okay d (Left y)
    cmp (Fail   _) (Okay d y) = Okay d (Right y)
    cmp (Fail   _) (Fail   y) = Fail y

-- | Alias of 'and'.
(.&.) :: Predicate a f t -> Predicate a f t' -> Predicate a f (t ::: t')
(.&.) = and

-- | Alias of 'or'.
(.|.) :: Predicate a f t -> Predicate a f t -> Predicate a f t
(.|.) = or

-- | Alias of 'orElse'.
(|||) :: Predicate a f t -> Predicate a f t' -> Predicate a f (Either t t')
(|||) = orElse

-- | Like 'fmap', but only maps the @Okay@ metadata to another result.
mapOkay :: (t -> Result f t') -> Predicate a f t -> Predicate a f t'
mapOkay f p a =
    case p a of
        Okay _ x -> f x
        Fail   x -> Fail x

-- | Like 'mapOkay', but for the @Fail@ case.
mapFail :: (f -> Result f' t) -> Predicate a f t -> Predicate a f' t
mapFail f p a =
    case p a of
        Fail   x -> f x
        Okay d x -> Okay d x

-- | A predicate modifier which makes the given predicate optional,
-- i.e. the @Okay@ metadata type becomes a 'Maybe' and in the failure-case
-- 'Nothing' is returned.
opt :: Predicate a f t -> Predicate a f (Maybe t)
opt = fmap (result (const $ return Nothing) (\d x -> Okay d (Just x)))

-- | A predicate modifier which returns as @Okay@ metadata the provided default
-- value if the given predicate fails.
def :: t -> Predicate a f t -> Predicate a f t
def t = fmap (result (const $ return t) Okay)

exec :: Predicate a f t -> a -> (f -> b) -> (t -> b) -> b
exec p a g f = case p a of
    Okay _ x -> f x
    Fail   x -> g x
