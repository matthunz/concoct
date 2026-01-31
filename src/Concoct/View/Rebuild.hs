{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Concoct.View.Rebuild
-- Copyright   : (c) Matt Hunzinger, 2026
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Concoct.View.Rebuild (Rebuild (..), runRebuild) where

import Concoct.View.Build
import Concoct.View.Internal
import Concoct.View.Skip
import Concoct.View.Unmount
import Control.Monad
import Control.Monad.State

newtype Rebuild m a = Rebuild {unRebuild :: StateT ViewState m a}
  deriving (Functor, Applicative, Monad)

instance (MonadIO m) => MonadView m (Rebuild m) where
  useState _ = Rebuild $ rebuildState
  useRef _ = Rebuild $ rebuildRef
  useEffect deps f = Rebuild $ do
    mdep <- gets $ peekStack . viewStack
    case mdep of
      Just oldDeps -> do
        deps' <- lift deps
        when (oldDeps /= deps') $ do
          modify $ \vs -> vs {viewStack = setStack deps' (viewStack vs)}
          lift $ f deps'
      Nothing -> error "useEffect: Dependencies not found in stack during rebuild"
  useOnUnmount _ = return ()
  component v = Rebuild $ rebuildComponent unRebuild v
  liftView = Rebuild . lift
  switchView cond vTrue vFalse = Rebuild $ do
    vs <- get
    let (mcond, s') = popStack (viewStack vs)
    case mcond of
      Just oldCond -> do
        cond' <- lift cond
        if oldCond == cond'
          then do
            put vs {viewStack = pushStack cond' s'}
            if cond' then unRebuild vTrue else unRebuild vFalse
          else do
            put vs {viewStack = pushStack cond' s'}
            if oldCond
              then unSkip vTrue
              else unSkip vFalse
            if cond'
              then unBuild vTrue
              else unBuild vFalse
      Nothing -> error "switchView: Condition not found in stack during rebuild"
  listView items f = Rebuild $ do
    vs <- get
    let (mOldItems, s') = popStack (viewStack vs)
    case mOldItems of
      Just oldItems -> do
        items' <- lift items
        put vs {viewStack = pushStack items' s'}
        -- Update shared items
        let common = zip oldItems items'
            oldLen = length oldItems
            newLen = length items'
        forM_ common $ \(old, new) ->
          if old == new
            then unSkip (f old)
            else do
              unSkip (f old)
              unBuild (f new)
        -- Unmount removed items
        when (oldLen > newLen) $
          forM_ (drop newLen oldItems) $ \old -> do
            vs' <- get
            (_, stack') <- lift $ runUnmount (f old) (viewStack vs')
            put vs' {viewStack = stack'}
        -- Build added items
        when (newLen > oldLen) $
          forM_ (drop oldLen items') $ \new ->
            unBuild (f new)
      Nothing -> error "listView: Items not found in stack during rebuild"

runRebuild :: (MonadIO m) => Rebuild m a -> ViewState -> m (a, ViewState)
runRebuild vr = runStateT (unRebuild vr)
