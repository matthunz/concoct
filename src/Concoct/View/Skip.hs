{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Concoct.View.Skip
-- Copyright   : (c) Matt Hunzinger, 2026
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Concoct.View.Skip (Skip (..), runSkip) where

import Concoct.View.Internal
import Control.Monad.State

newtype Skip m a = Skip {unSkip :: StateT ViewState m a}
  deriving (Functor, Applicative, Monad)

instance (MonadIO m) => MonadView m (Skip m) where
  useState _ = Skip rebuildState
  useRef _ = Skip rebuildRef
  useEffect _ _ = return ()
  useOnUnmount _ = return ()
  component v = Skip $ rebuildComponent unSkip v
  liftView _ = return ()
  switchView _ vTrue vFalse = Skip $ do
    vs <- get
    let (mcond, s') = popStack (viewStack vs)
    case mcond of
      Just cond' -> do
        put vs {viewStack = pushStack cond' s'}
        if cond' then unSkip vTrue else unSkip vFalse
      Nothing -> error "switchView: Condition not found in stack during skip"
  listView _ f = Skip $ do
    vs <- get
    let (mItems, s') = popStack @[_] (viewStack vs)
    case mItems of
      Just items' -> do
        put vs {viewStack = pushStack items' s'}
        mapM_ (unSkip . f) items'
      Nothing -> error "listView: Items not found in stack during skip"

runSkip :: (MonadIO m) => Skip m a -> ViewState -> m (a, ViewState)
runSkip vs = runStateT (unSkip vs)
