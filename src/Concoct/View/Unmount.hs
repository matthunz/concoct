{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Concoct.View.Unmount
-- Copyright   : (c) Matt Hunzinger, 2026
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Concoct.View.Unmount (Unmount (..), runUnmount) where

import Concoct.View.Internal
import Control.Monad.State

newtype Unmount m a = Unmount {unUnmount :: StateT Stack m a}
  deriving (Functor, Applicative, Monad)

instance (MonadIO m) => MonadView m (Unmount m) where
  useState _ = Unmount $ do
    stack <- get
    let (ref, stack') = rebuildState' stack
    put stack'
    return ref
  useRef _ = Unmount $ do
    stack <- get
    let (ref, stack') = rebuildRef' stack
    put stack'
    return ref
  useEffect _ _ = Unmount $ modify skipStack
  useOnUnmount = Unmount . lift
  component v = Unmount $ do
    modify skipStack
    unUnmount v
  liftView _ = return ()
  switchView _ vTrue vFalse = Unmount $ do
    stack <- get
    let (mcond, stack') = popStack stack
    put stack'
    case mcond of
      Just cond' -> if cond' then unUnmount vTrue else unUnmount vFalse
      Nothing -> error "switchView: Condition not found in stack during unmount"
  listView _ f = Unmount $ do
    stack <- get
    let (mItems, stack') = popStack @[_] stack
    put stack'
    case mItems of
      Just items' -> mapM_ (unUnmount . f) items'
      Nothing -> error "listView: Items not found in stack during unmount"

runUnmount :: (MonadIO m) => Unmount m a -> Stack -> m (a, Stack)
runUnmount vu = runStateT (unUnmount vu)
