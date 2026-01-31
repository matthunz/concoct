{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Concoct.View.Tree
-- Copyright   : (c) Matt Hunzinger, 2026
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Concoct.View.Tree
  ( ViewTree (..),
    viewTree,
    rebuildViewTree,
    unmountViewTree,
  )
where

import Concoct.View.Build
import Concoct.View.Internal
import Concoct.View.Rebuild
import Concoct.View.Skip
import Concoct.View.Unmount
import Control.Monad.IO.Class
import Data.IORef

-- | View tree.
data ViewTree t = ViewTree
  { viewTreeView :: forall m. (MonadView t m) => m (),
    viewTreeStack :: Stack,
    viewTreeChanged :: IORef Bool,
    viewTreePendingUpdates :: IORef (IO ())
  }

-- | Create a view tree from a view.
viewTree :: (MonadIO t) => (forall m. (MonadView t m) => m ()) -> t (ViewTree t)
viewTree v = do
  changedRef <- liftIO $ newIORef False
  pendingRef <- liftIO $ newIORef (pure ())
  let updater m = modifyIORef pendingRef (>> m) >> writeIORef changedRef True
      s = ViewState emptyStack updater
  (_, s') <- runBuild v s
  return (ViewTree v (flushStack $ viewStack s') changedRef pendingRef)

-- | Rebuild the view tree if changed.
rebuildViewTree :: (MonadIO t) => ViewTree t -> t (ViewTree t)
rebuildViewTree t = do
  pending <- liftIO $ readIORef (viewTreePendingUpdates t)
  liftIO pending
  liftIO $ writeIORef (viewTreePendingUpdates t) (pure ())
  changed <- liftIO $ readIORef (viewTreeChanged t)
  if changed
    then do
      liftIO $ writeIORef (viewTreeChanged t) False
      let updater m = modifyIORef (viewTreePendingUpdates t) (>> m) >> writeIORef (viewTreeChanged t) True
          s = ViewState (viewTreeStack t) updater
      (_, s') <- runRebuild (viewTreeView t) s
      return t {viewTreeStack = flushStack $ viewStack s'}
    else do
      let updater m = modifyIORef (viewTreePendingUpdates t) (>> m) >> writeIORef (viewTreeChanged t) True
          s = ViewState (viewTreeStack t) updater
      (_, s') <- runSkip (viewTreeView t) s
      return t {viewTreeStack = flushStack $ viewStack s'}

-- | Unmount the view tree.
unmountViewTree :: (MonadIO t) => ViewTree t -> t (ViewTree t)
unmountViewTree t = do
  (_, stack') <- runUnmount (viewTreeView t) (viewTreeStack t)
  return t {viewTreeStack = stack'}
