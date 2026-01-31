{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Concoct.View.Internal
-- Copyright   : (c) Matt Hunzinger, 2026
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Concoct.View.Internal
  ( -- * MonadView
    MonadView (..),

    -- * StateRef
    StateRef (..),
    readStateRef,
    writeStateRef,

    -- * ViewRef
    ViewRef (..),
    readViewRef,

    -- * Stack
    Stack (..),
    emptyStack,
    pushStack,
    popStack,
    peekStack,
    setStack,
    skipStack,
    flushStack,

    -- * ViewState
    ViewState (..),
    rebuildState,
    rebuildState',
    rebuildRef,
    rebuildRef',
    rebuildComponent,
  )
where

import Control.Monad
import Control.Monad.State
import Data.Dynamic
import Data.IORef

-- | Monadic view.
--
-- Views are interpreted in multiple passes:
--
-- * Build
-- * Rebuild
-- * Skip
-- * Unmount
--
-- And can be extended for further passes, such as layout or render.
--
-- Hooks provide access to values in a view across passes.
--
-- Views are seperated into @component@s, with each providing a scope for updates to occur.
-- A @component@ will only be rebuilt if its state is changed.
-- If a component containing children is updated, its children are rebuilt as well.
class (Monad m) => MonadView t m | m -> t where
  -- | Hook to use a mutable state reference.
  useState :: (Typeable a) => t a -> m (StateRef a)

  -- | Hook to use a constant view reference.
  useRef :: (Typeable a) => t a -> m (ViewRef a)

  -- | Hook to use an effect that runs when dependencies change.
  useEffect :: (Eq d, Typeable d) => t d -> (d -> t ()) -> m ()

  -- | Hook that runs when the current component is unmounted.
  useOnUnmount :: t () -> m ()

  -- | Component view.
  component :: (forall x. (MonadView t x) => x ()) -> m ()

  -- | Lift a monadic action into the view.
  liftView :: t () -> m ()

  -- | Conditional view.
  -- This will render the first view if the condition is @True@, otherwise the second.
  switchView ::
    t Bool ->
    (forall x. (MonadView t x) => x ()) ->
    (forall x. (MonadView t x) => x ()) ->
    m ()

  -- | List view.
  -- This will render a view for each item in the list.
  listView :: (Typeable a, Eq a) => t [a] -> (a -> (forall x. (MonadView t x) => x ())) -> m ()

-- | State reference.
-- Created with @useState@.
data StateRef a = StateRef
  { stateRef :: IORef a,
    stateRefUpdater :: IO () -> IO ()
  }

-- | Read a state reference.
readStateRef :: (MonadIO m) => StateRef a -> m a
readStateRef = liftIO . readIORef . stateRef

-- | Write to a state reference.
-- This will schedule an update to the current @component@.
writeStateRef :: (MonadIO m) => StateRef a -> a -> m ()
writeStateRef ref = liftIO . stateRefUpdater ref . writeIORef (stateRef ref)

-- | View reference.
-- Created with @useRef@.
newtype ViewRef a = ViewRef a

-- | Read a view reference.
readViewRef :: (Applicative m) => ViewRef a -> m a
readViewRef (ViewRef a) = pure a

data Stack = Stack
  { stackBefore :: [Dynamic],
    stackAfter :: [Dynamic]
  }

emptyStack :: Stack
emptyStack = Stack [] []

pushStack :: (Typeable a) => a -> Stack -> Stack
pushStack a (Stack before after) = Stack (toDyn a : before) after

popStack :: (Typeable a) => Stack -> (Maybe a, Stack)
popStack (Stack before []) = (Nothing, Stack before [])
popStack (Stack before (d : ds)) =
  case fromDynamic d of
    Just a -> (Just a, Stack (d : before) ds)
    Nothing -> popStack (Stack (d : before) ds)

peekStack :: (Typeable a) => Stack -> Maybe a
peekStack (Stack _ []) = Nothing
peekStack (Stack _ (d : _)) = fromDynamic d

setStack :: (Typeable a) => a -> Stack -> Stack
setStack a (Stack before (_ : ds)) = Stack (toDyn a : before) ds
setStack _ s = s

skipStack :: Stack -> Stack
skipStack (Stack before (d : after)) = Stack (d : before) after
skipStack s = s

flushStack :: Stack -> Stack
flushStack (Stack before after) = Stack [] (reverse before ++ after)

data ViewState = ViewState
  { viewStack :: Stack,
    viewUpdater :: IO () -> IO ()
  }

rebuildState :: (MonadIO m, Typeable a) => StateT ViewState m (StateRef a)
rebuildState = do
  vs <- get
  let (ref, s') = rebuildState' (viewStack vs)
  put vs {viewStack = s'}
  return ref

rebuildState' :: (Typeable a) => Stack -> (StateRef a, Stack)
rebuildState' s = do
  let (mref, s') = popStack s
  case mref of
    Just ref -> (ref, s')
    Nothing -> error "useState: StateRef not found in stack during rebuild"

rebuildRef :: (Monad m, Typeable a) => StateT ViewState m (ViewRef a)
rebuildRef = do
  vs <- get
  let (ref, s') = rebuildRef' (viewStack vs)
  put vs {viewStack = s'}
  return ref

rebuildRef' :: (Typeable a) => Stack -> (ViewRef a, Stack)
rebuildRef' s = do
  let (mref, s') = popStack s
  case mref of
    Just ref -> (ref, s')
    Nothing -> error "useRef: ViewRef not found in stack during rebuild"

rebuildComponent ::
  (MonadIO m, MonadView t f) =>
  (f () -> StateT ViewState m ()) ->
  (forall x. (MonadView t x) => x ()) ->
  StateT ViewState m ()
rebuildComponent g v = do
  vs <- get
  let (mref, s') = popStack (viewStack vs)
  put vs {viewStack = s'}
  case mref of
    Just ref -> do
      changed <- liftIO $ readIORef ref
      when changed $ liftIO $ writeIORef ref False
      g v
      vs' <- get
      put vs' {viewUpdater = viewUpdater vs}
    Nothing -> error "component: Change ref not found in stack during rebuild"
