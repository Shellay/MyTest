module Hello where


map2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
map2 f ma mb = do a <- ma
                  b <- mb
                  return $ f a b


res = map2 (+) (Just 1) (Just 2)


main :: IO ()
main = putStrLn $ show res
