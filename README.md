# Concoct

A declarative user-interface framework for Haskell

```hs
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
```

## Prior art

- [ReactJS](https://react.dev/)
- [Reflex](https://reflex-frp.org/)
