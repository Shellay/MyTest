{-# LANGUAGE RankNTypes #-}

newtype ChurchList a
  = ChurchList { runList :: forall r. (a -> r -> r) -> r -> r }

fromList xs = ChurchList $ \k z -> foldr 
