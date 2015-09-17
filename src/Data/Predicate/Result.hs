-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeOperators #-}

module Data.Predicate.Result where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Prelude

-- | A 'Bool'-like type where each branch--@Fail@ and @Okay@--carries
-- some metadata.
data Result f t
    = Fail f
    | Okay !Double t
    deriving (Eq, Ord, Show)

instance Functor (Result f) where
    fmap f (Okay d x) = Okay d (f x)
    fmap _ (Fail   x) = Fail x

instance Applicative (Result f) where
    pure  = return
    (<*>) = ap

instance Monad (Result f) where
    return           = Okay 0
    (Okay _ x) >>= k = k x
    (Fail   x) >>= _ = Fail x

result :: (f -> a) -> (Double -> t -> a) -> Result f t -> a
result f _ (Fail   x) = f x
result _ g (Okay d x) = g d x

fromEither :: Either f t -> Result f t
fromEither = either Fail return

toEither :: Result f t -> Either f t
toEither = result Left (\_ x -> Right x)

newtype ResultT f m t = ResultT { runResultT :: m (Result f t) }

instance Monad m => Functor (ResultT f m) where
    fmap f = ResultT . liftM (fmap f) . runResultT

instance Monad m => Applicative (ResultT f m) where
    pure  = return
    (<*>) = ap

instance Monad m => Monad (ResultT f m) where
    return  = ResultT . return . return
    m >>= k = ResultT $ runResultT m >>= \a -> case a of
        Okay _ x -> runResultT (k x)
        Fail   x -> return (Fail x)
    fail = ResultT . fail

instance MonadTrans (ResultT f) where
    lift = ResultT . liftM return

instance MonadIO m => MonadIO (ResultT f m) where
    liftIO = lift . liftIO

resultT :: Monad m => (f -> m a) -> (Double -> t -> m a) -> ResultT f m t -> m a
resultT f g (ResultT m) = m >>= \a -> case a of
    Fail   x -> f x
    Okay d x -> g d x

resultT' :: Monad m => (f -> m a) -> (t -> m a) -> ResultT f m t -> m a
resultT' f g = resultT f (\_ x -> g x)

mapResultT :: (m (Result f t) -> n (Result f' t')) -> ResultT f m t -> ResultT f' n t'
mapResultT f m = ResultT $ f (runResultT m)

hoistResult :: Monad m => Result f t -> ResultT f m t
hoistResult = ResultT . return

okay :: Monad m => Double -> t -> ResultT f m t
okay d = hoistResult . Okay d

throwF :: Monad m => f -> ResultT f m t
throwF = hoistResult . Fail

