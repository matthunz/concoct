{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

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
    runViewBuilder,
    ViewRebuilder (..),
    runViewRebuilder,
    ViewSkipper (..),
    runViewSkipper,
    ViewUnmounter (..),
    runViewUnmounter,

    -- * ViewState
    ViewState (..),

    -- * ViewTree
    ViewTree (..),
    viewTree,
    rebuildViewTree,
    unmountViewTree,
  )
where

import Control.Monad
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

  useEffect :: (Eq d, Typeable d) => t d -> (d -> t ()) -> m ()

  useOnUnmount :: t () -> m ()

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
  useEffect deps f = ViewBuilder $ do
    deps' <- lift deps
    modify $ \vs -> vs {viewStack = pushStack deps' (viewStack vs)}
    lift $ f deps'
  useOnUnmount _ = return ()
  component vb = ViewBuilder $ do
    ref <- liftIO $ newIORef False
    let updater = writeIORef ref True
    vs <- get
    put vs {viewUpdater = updater, viewStack = pushStack ref (viewStack vs)}
    unViewBuilder vb
    vs' <- get
    put vs' {viewUpdater = viewUpdater vs}
  liftView = ViewBuilder . lift

runViewBuilder :: (MonadIO m) => ViewBuilder m a -> ViewState -> m (a, ViewState)
runViewBuilder vb = runStateT (unViewBuilder vb)

newtype ViewRebuilder m a = ViewRebuilder {unViewRebuilder :: StateT ViewState m a}
  deriving (Functor, Applicative, Monad)

instance (MonadIO m) => MonadView m (ViewRebuilder m) where
  useState _ = ViewRebuilder $ rebuildState
  useEffect deps f = ViewRebuilder $ do
    mdep <- gets $ peekStack . viewStack
    case mdep of
      Just oldDeps -> do
        deps' <- lift deps
        when (oldDeps /= deps') $ do
          modify $ \vs -> vs {viewStack = setStack deps' (viewStack vs)}
          lift $ f deps'
      Nothing -> error "useEffect: Dependencies not found in stack during rebuild"
  useOnUnmount _ = return ()
  component v = ViewRebuilder $ rebuildComponent unViewRebuilder unViewRebuilder v
  liftView = ViewRebuilder . lift

runViewRebuilder :: (MonadIO m) => ViewRebuilder m a -> ViewState -> m (a, ViewState)
runViewRebuilder vr = runStateT (unViewRebuilder vr)

newtype ViewSkipper m a = ViewSkipper {unViewSkipper :: StateT ViewState m a}
  deriving (Functor, Applicative, Monad)

instance (MonadIO m) => MonadView m (ViewSkipper m) where
  useState _ = ViewSkipper rebuildState
  useEffect _ _ = return ()
  useOnUnmount _ = return ()
  component v = ViewSkipper $ rebuildComponent unViewRebuilder unViewSkipper v
  liftView _ = return ()

runViewSkipper :: (MonadIO m) => ViewSkipper m a -> ViewState -> m (a, ViewState)
runViewSkipper vs = runStateT (unViewSkipper vs)

newtype ViewUnmounter m a = ViewUnmounter {unViewUnmounter :: StateT Stack m a}
  deriving (Functor, Applicative, Monad)

instance (MonadIO m) => MonadView m (ViewUnmounter m) where
  useState _ = ViewUnmounter $ do
    stack <- get
    let (ref, stack') = rebuildState' stack
    put stack'
    return ref
  useEffect _ _ = ViewUnmounter $ modify skipStack
  useOnUnmount = ViewUnmounter . lift
  component v = ViewUnmounter $ do
    modify skipStack
    unViewUnmounter v
  liftView _ = return ()

runViewUnmounter :: (MonadIO m) => ViewUnmounter m a -> Stack -> m (a, Stack)
runViewUnmounter vu = runStateT (unViewUnmounter vu)

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

rebuildComponent ::
  (MonadIO m, MonadView t f, MonadView t g) =>
  (g () -> StateT ViewState m ()) ->
  (f () -> StateT ViewState m ()) ->
  (forall x. (MonadView t x) => x ()) ->
  StateT ViewState m ()
rebuildComponent f g v = do
  vs <- get
  let (mref, s') = popStack (viewStack vs)
  put vs {viewStack = s'}
  case mref of
    Just ref -> do
      changed <- liftIO $ readIORef ref
      if changed
        then do
          liftIO $ writeIORef ref False
          f v
        else g v
      vs' <- get
      put vs' {viewUpdater = viewUpdater vs}
    Nothing -> error "component: Change ref not found in stack during rebuild"

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

unmountViewTree :: (MonadIO t) => ViewTree t -> t (ViewTree t)
unmountViewTree t = do
  (_, stack') <- runViewUnmounter (viewTreeView t) (viewTreeStack t)
  return t {viewTreeStack = stack'}
