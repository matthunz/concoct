{-# LANGUAGE FlexibleContexts #-}

module Main where

import Concoct

counter :: (MonadView IO m) => m ()
counter = do
  count <- useState $ pure (0 :: Int)

  useEffect (readStateRef count) $ \c -> do
    putStrLn $ "Count: " ++ show c
    writeStateRef count $ c + 1

  switchView
    (even <$> readStateRef count)
    (liftView $ putStrLn "Even!")
    (liftView $ putStrLn "Odd!")

  useOnUnmount $ do
    c <- readStateRef count
    putStrLn $ "Unmount:" ++ show c

main :: IO ()
main = do
  t <- viewTree counter
  t' <- rebuildViewTree t
  _ <- unmountViewTree t'
  return ()
