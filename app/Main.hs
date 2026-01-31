{-# LANGUAGE FlexibleContexts #-}

module Main where

import Concoct

counter :: (MonadView IO m) => m ()
counter = do
  count <- useState $ pure (0 :: Int)

  liftView $ do
    c <- readStateRef count
    putStrLn $ "Count: " ++ show c
    writeStateRef count $ c + 1

main :: IO ()
main = do
  t <- viewTree counter
  _ <- rebuildViewTree t
  return ()
