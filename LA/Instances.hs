{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module LA.Instances () where

import Data.Vector.Fixed
import LA.Algebra
import Prelude hiding (foldl, zipWith, map, replicate)





instance (Additive a, Vector v a) => Additive (v a) where
  add  = zipWith add    ; {-# INLINE add #-}
  neg  = map neg        ; {-# INLINE neg #-}
  zero = replicate zero ; {-# INLINE zero #-}

instance (Multiplicative a, Vector v a) => Multiplicative (v a) where
  mul = zipWith mul   ; {-# INLINE mul #-}
  one = replicate one ; {-# INLINE one #-}

instance (Additive a, Multiplicative a, Vector v a) => VectorSpace (v a) where
    type Scalar (v a) = a
    scale = flip (map . mul) ; {-# INLINE scale #-}

instance (Additive a, Multiplicative a, Vector v a) => InnerSpace (v a) where
    dot a = foldl add zero . (a .*)









