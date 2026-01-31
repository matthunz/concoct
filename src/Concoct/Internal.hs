{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Concoct.Internal
  ( -- * MonadView
    MonadView (..),

    -- * StateRef
    StateRef (..),
    readStateRef,
    writeStateRef,

    -- * Stack
    Stack (..),
    emptyStack,
    pushStack,

    -- * View passes
    ViewBuilder (..),
    ViewRebuilder (..),
    ViewSkipper (..),

    -- * ViewState
    ViewState (..),

    -- * ViewTree
    ViewTree (..),
    viewTree,
    rebuildViewTree,
  )
where

import Control.Monad.State
import Data.Dynamic
import Data.IORef

data StateRef a = StateRef
  { stateRef :: IORef a,
    stateRefUpdater :: IO ()
  }

readStateRef :: (MonadIO m) => StateRef a -> m a
readStateRef = liftIO . readIORef . stateRef

writeStateRef :: (MonadIO m) => StateRef a -> a -> m ()
writeStateRef ref a = liftIO $ do
  writeIORef (stateRef ref) a
  stateRefUpdater ref

class (Monad m) => MonadView t m | m -> t where
  useState :: (Typeable a) => t a -> m (StateRef a)

  component :: (forall x. (MonadView t x) => x ()) -> m ()

  liftView :: t () -> m ()

data Stack = Stack
  { stackBefore :: [Dynamic],
    stackAfter :: [Dynamic]
  }

emptyStack :: Stack
emptyStack = Stack [] []

pushStack :: (Typeable a) => a -> Stack -> Stack
pushStack a (Stack before after) = Stack (toDyn a : before) after

popStack :: (Typeable a) => Stack -> (Maybe a, Stack)
popStack (Stack [] after) = (Nothing, Stack [] after)
popStack (Stack (d : ds) after) =
  case fromDynamic d of
    Just a -> (Just a, Stack ds after)
    Nothing -> popStack (Stack ds after)

flushStack :: Stack -> Stack
flushStack (Stack before after) = Stack (reverse before ++ after) []

data ViewState = ViewState
  { viewStack :: Stack,
    viewUpdater :: IO ()
  }

newtype ViewBuilder m a = ViewBuilder {unViewBuilder :: StateT ViewState m a}
  deriving (Functor, Applicative, Monad)

instance (MonadIO m) => MonadView m (ViewBuilder m) where
  useState a = ViewBuilder $ do
    a' <- lift a
    ref <- liftIO $ newIORef a'
    vs <- get
    let sRef = StateRef ref $ viewUpdater vs
    put vs {viewStack = pushStack sRef (viewStack vs)}
    return sRef
  component vb = ViewBuilder $ do
    ref <- liftIO $ newIORef False
    let updater = writeIORef ref True
    vs <- get
    put vs {viewUpdater = updater, viewStack = pushStack ref (viewStack vs)}
    unViewBuilder vb
    vs' <- get
    put vs' {viewUpdater = viewUpdater vs}
  liftView = ViewBuilder . lift

newtype ViewRebuilder m a = ViewRebuilder {unViewRebuilder :: StateT ViewState m a}
  deriving (Functor, Applicative, Monad)

instance (MonadIO m) => MonadView m (ViewRebuilder m) where
  useState _ = ViewRebuilder $ rebuildState
  component vb = ViewRebuilder $ do
    vs <- get
    let (mref, s') = popStack (viewStack vs)
    put vs {viewStack = s'}
    case mref of
      Just ref -> do
        liftIO $ writeIORef ref False
        unViewRebuilder vb
        vs' <- get
        put vs' {viewUpdater = viewUpdater vs}
      Nothing -> error "component: Change ref not found in stack during rebuild"
  liftView = ViewRebuilder . lift

newtype ViewSkipper m a = ViewSkipper {unViewSkipper :: StateT ViewState m a}
  deriving (Functor, Applicative, Monad)

instance (MonadIO m) => MonadView m (ViewSkipper m) where
  useState _ = ViewSkipper rebuildState
  component vb = ViewSkipper $ do
    vs <- get
    let (mref, s') = popStack (viewStack vs)
    put vs {viewStack = s'}
    case mref of
      Just ref -> do
        changed <- liftIO $ readIORef ref
        if changed
          then do
            liftIO $ writeIORef ref False
            unViewRebuilder vb
          else unViewSkipper vb
        vs' <- get
        put vs' {viewUpdater = viewUpdater vs}
      Nothing -> error "component: Change ref not found in stack during rebuild"
  liftView _ = return ()

rebuildState :: (MonadIO m, Typeable a) => StateT ViewState m (StateRef a)
rebuildState = do
  vs <- get
  let (mref, s') = popStack (viewStack vs)
  put vs {viewStack = s'}
  case mref of
    Just ref -> return ref
    Nothing -> error "useState: StateRef not found in stack during rebuild"

data ViewTree t = ViewTree
  { viewTreeView :: forall m. (MonadView t m) => m (),
    viewTreeStack :: Stack,
    viewTreeChanged :: IORef Bool
  }

viewTree :: (MonadIO t) => (forall m. (MonadView t m) => m ()) -> t (ViewTree t)
viewTree v = do
  ref <- liftIO $ newIORef False
  let updater = writeIORef ref True
      s = ViewState emptyStack updater
  (_, s') <- runStateT (unViewBuilder v) s
  return (ViewTree v (flushStack $ viewStack s') ref)

rebuildViewTree :: (MonadIO t) => ViewTree t -> t (ViewTree t)
rebuildViewTree t = do
  changed <- liftIO $ readIORef (viewTreeChanged t)
  if changed
    then do
      let updater = writeIORef (viewTreeChanged t) False
          s = ViewState (viewTreeStack t) updater
      (_, s') <- runStateT (unViewRebuilder $ viewTreeView t) s
      return (ViewTree (viewTreeView t) (flushStack $ viewStack s') (viewTreeChanged t))
    else do
      let updater = writeIORef (viewTreeChanged t) False
          s = ViewState (viewTreeStack t) updater
      (_, s') <- runStateT (unViewSkipper $ viewTreeView t) s
      return (ViewTree (viewTreeView t) (flushStack $ viewStack s') (viewTreeChanged t))
