{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UndecidableInstances  #-}

{- |
Module                  : Unlift
Copyright               : (c) 2021 Kowainik
                          (c) 2017 FP Complete
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

This module provides 'MonadUnlift' typeclass and functions to work with it.

See 'MonadUnlift' documentation for more information about its purpose.

@since 0.0.0.0
-}
module Unlift
    ( MonadUnlift (..)
    , defaultWithRunInBase
    , askRunInBase
    , Unlift (..)
    , askUnlift
    ) where

import Control.Monad.Base (MonadBase)
import Control.Monad.ST (ST)
import Control.Monad.STM (STM)
import Control.Monad.Trans.Identity (IdentityT (..))
import Control.Monad.Trans.Reader (ReaderT (..))


{- | Typeclass to allow actions in monadic context @m@ to be run in the base monad @b@.

This typeclass is similar to @MonadUnliftIO@ from the
<http://hackage.haskell.org/package/unliftio unliftio> package. However
'MonadUnlift' works with any base monad, not only 'IO'.

This typeclass is helpful when writing code that is polymorphic over the base
monad, so later you can select a different base monad for each specific use-case.

While you can use @lift@ to allow some action to be lifted into another
monad, this class captures the opposite concept.

Instances of this typeclass should satisfy the following laws:

* __Distributivity:__

    @
    'withRunInBase' (\\run -> run f >> run g)
        ≡
    'withRunInBase' (\\run -> run f) >> 'withRunInBase' (\\run -> run g)
    @

* __Identity:__

    @
    'askUnlift' >>= \\u -> ('Control.Monad.Base.liftBase' . 'runUnlift' u) m ≡ m
    @

@since 0.0.0.0
-}
class (MonadBase b m) => MonadUnlift b m
  where
    {- | Convenient function to capture the monadic context @m@ and run the @b@
    action with a runner function. The runner function is used to run a monadic
    action @m@ in the base monad @b@.

    @since 0.0.0.0
    -}
    withRunInBase :: ((forall a . m a -> b a) -> b x) -> m x

{- |
@since 0.0.0.0
-}
instance MonadUnlift IO IO
  where
    withRunInBase :: ((forall a . IO a -> IO a) -> IO x) -> IO x
    withRunInBase = \run -> run id

{- |
@since 0.0.0.0
-}
instance MonadUnlift STM STM
  where
    withRunInBase :: ((forall a . STM a -> STM a) -> STM x) -> STM x
    withRunInBase = \run -> run id

{- |
@since 0.0.0.0
-}
instance MonadUnlift (ST s) (ST s)
  where
    withRunInBase :: ((forall a . ST s a -> ST s a) -> ST s x) -> ST s x
    withRunInBase = \run -> run id

{- |
@since 0.0.0.0
-}
instance (MonadUnlift b m) => MonadUnlift b (IdentityT m)
  where
    withRunInBase :: ((forall a . IdentityT m a -> b a) -> b x) -> IdentityT m x
    withRunInBase = defaultWithRunInBase IdentityT runIdentityT


{- |
@since 0.0.0.0
-}
instance (MonadUnlift b m) => MonadUnlift b (ReaderT r m)
  where
    withRunInBase :: ((forall a . ReaderT r m a -> b a) -> b x) -> ReaderT r m x
    withRunInBase runWithReader = ReaderT $ \env ->
        withRunInBase (\maToba -> runWithReader (maToba . flip runReaderT env))

{- | A helper function for implementing @MonadUnlift@ instances.

Useful for the common case where you want to simply delegate to the
underlying transformer in @newtype@s.

__Example:__

@
__newtype__ AppT m a = AppT
    { unAppT :: 'ReaderT' Int m a
    } __deriving newtype__ ('Functor', 'Applicative', 'Monad')

__instance__ ('MonadUnlift' b m) => 'MonadUnlift' b (AppT m)
  __where__
    'withRunInBase' = 'defaultWithRunInBase' AppT unAppT
@

@since 0.0.0.0
-}
defaultWithRunInBase
    :: (MonadUnlift b n)
    => (n x -> m x)  -- ^ Wrapper
    -> (forall a . m a -> n a)  -- ^ Unwrapper
    -> ((forall a . m a -> b a) -> b x)  -- ^ Action to do in base monad
    -> m x  -- ^ Result in unlifted monad
defaultWithRunInBase wrap unwrap run =
    wrap $ withRunInBase (\ma2ba -> run (ma2ba . unwrap))

{- | Capture the current monadic context @m@, providing the ability to
run monadic actions in the base monad @b@.

Useful when you need to apply on one concrete type.

__Note:__ If you run into issues when using this function, most likely that
you need 'askUnlift' instead.

@since 0.0.0.0
-}
askRunInBase :: (MonadUnlift b m) => m (m a -> b a)
askRunInBase = withRunInBase pure

{- | Polymorphic wrapper over the function returned by 'withRunInBase'.

Use 'askUnlift' instead of 'askRunInBase' when you need to use the return
@unlift@ with variables of different types.

@since 0.0.0.0
-}
newtype Unlift b m = Unlift
    { runUnlift :: forall x . m x -> b x
    }

{- | Similar to 'askRunInBase', but works with the 'Unlift' wrapper.

Use 'askUnlift' instead of 'askRunInBase' when you need to use the return
@unlift@ with variables of different types.

@since 0.0.0.0
-}
askUnlift :: (MonadUnlift b m) => m (Unlift b m)
askUnlift = withRunInBase $ \runInBase -> pure (Unlift runInBase)
