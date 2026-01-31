-- |
-- Module      : Concoct.View
-- Copyright   : (c) Matt Hunzinger, 2026
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Concoct.View
  ( -- * MonadView
    MonadView (..),

    -- * StateRef
    StateRef,
    readStateRef,
    writeStateRef,

    -- * ViewRef
    ViewRef,
    readViewRef,

    -- * ViewTree
    ViewTree,
    viewTree,
    rebuildViewTree,
    unmountViewTree,
  )
where

import Concoct.View.Internal
import Concoct.View.Tree
