{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Concoct.View.Build
-- Copyright   : (c) Matt Hunzinger, 2026
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Concoct.View.Build (Build (..), runBuild) where

import Concoct.View.Internal
import Control.Monad.State
import Data.IORef

newtype Build m a = Build {unBuild :: StateT ViewState m a}
  deriving (Functor, Applicative, Monad)

instance (MonadIO m) => MonadView m (Build m) where
  useState a = Build $ do
    a' <- lift a
    ref <- liftIO $ newIORef a'
    vs <- get
    let sRef = StateRef ref $ viewUpdater vs
    put vs {viewStack = pushStack sRef (viewStack vs)}
    return sRef
  useRef a = Build $ do
    a' <- lift a
    let vRef = ViewRef a'
    modify $ \vs -> vs {viewStack = pushStack vRef (viewStack vs)}
    return vRef
  useEffect deps f = Build $ do
    deps' <- lift deps
    modify $ \vs -> vs {viewStack = pushStack deps' (viewStack vs)}
    lift $ f deps'
  useOnUnmount _ = return ()
  component vb = Build $ do
    ref <- liftIO $ newIORef False
    let updater m = writeIORef ref True >> m
    vs <- get
    put vs {viewUpdater = updater, viewStack = pushStack ref (viewStack vs)}
    unBuild vb
    vs' <- get
    put vs' {viewUpdater = viewUpdater vs}
  liftView = Build . lift
  switchView cond vTrue vFalse = Build $ do
    cond' <- lift cond
    modify $ \vs -> vs {viewStack = pushStack cond' (viewStack vs)}
    if cond' then unBuild vTrue else unBuild vFalse
  listView items f = Build $ do
    items' <- lift items
    modify $ \vs -> vs {viewStack = pushStack items' (viewStack vs)}
    mapM_ (unBuild . f) items'

runBuild :: (MonadIO m) => Build m a -> ViewState -> m (a, ViewState)
runBuild vb = runStateT (unBuild vb)
