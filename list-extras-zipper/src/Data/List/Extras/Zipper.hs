module Data.List.Extras.Zipper
  ( zipper
  ) where

zipper :: [a] -> [([a], a, [a])]
zipper [] = []
zipper (r0:rs0) = let z0 = ([], r0, rs0) in z0 : go z0
  where
    go :: ([a], a, [a]) -> [([a], a, [a])]
    go ( _, _,   []) = []
    go (ls, l, r:rs) = let z = (l:ls, r, rs) in z : go z
    {-# INLINE go #-}
{-# INLINE zipper #-}
