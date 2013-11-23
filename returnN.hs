{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
  FlexibleContexts, OverlappingInstances, TypeFamilies #-}


class Monad m => Ret a m b where
    returnN :: a -> m b

instance (Monad m, a ~ b) => Ret a m b where
    returnN = return

instance (Monad m, Monad n, Ret a m b) => Ret a n (m b) where
    returnN = return . returnN


boo :: [[[Maybe [Either () [Int]]]]]
boo = returnN 0
