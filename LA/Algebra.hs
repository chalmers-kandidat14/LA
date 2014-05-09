{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module LA.Algebra where

import LA.Epsilon

infix  7 *.*
infixl 7 .%,%.
infixl 6 .*
infixl 5 .+,.-

-- An additive group
class Additive a where
  zero :: a
  add  :: a -> a -> a
  neg  :: a -> a

sub :: Additive a => a -> a -> a
sub a b = a `add` neg b

(.+), (.-) :: Additive a => a -> a -> a
(.+) = add ; {-# INLINE (.+) #-}
(.-) = sub ; {-# INLINE (.-) #-}

instance Additive Int where
  zero = 0    ; {-# INLINE zero #-}
  add  = (+)  ; {-# INLINE add #-}
  neg  = (0-) ; {-# INLINE neg #-}

instance Additive Integer where
  zero = 0    ; {-# INLINE zero #-}
  add  = (+)  ; {-# INLINE add #-}
  neg  = (0-) ; {-# INLINE neg #-}

instance Additive Float where
  zero = 0    ; {-# INLINE zero #-}
  add  = (+)  ; {-# INLINE add #-}
  neg  = (0-) ; {-# INLINE neg #-}

instance Additive Double where
  zero = 0    ; {-# INLINE zero #-}
  add  = (+)  ; {-# INLINE add #-}
  neg  = (0-) ; {-# INLINE neg #-}



-- An multiplicative semigroup
class Multiplicative a where
  one :: a
  mul :: a -> a -> a

(.*) :: Multiplicative a => a -> a -> a
(.*) = mul ; {-# INLINE (.*) #-}


instance Multiplicative Int where
  one = 1   ; {-# INLINE one #-}
  mul = (*) ; {-# INLINE mul #-}

instance Multiplicative Integer where
  one = 1   ; {-# INLINE one #-}
  mul = (*) ; {-# INLINE mul #-}

instance Multiplicative Float where
  one = 1   ; {-# INLINE one #-}
  mul = (*) ; {-# INLINE mul #-}

instance Multiplicative Double where
  one = 1   ; {-# INLINE one #-}
  mul = (*) ; {-# INLINE mul #-}



-- A vector space
class Additive a => VectorSpace a where
  type Scalar a :: *
  scale :: a -> Scalar a -> a

(.%) :: VectorSpace a => a -> Scalar a -> a
(.%) = scale ; {-# INLINE (.%) #-}

(%.) :: VectorSpace a => Scalar a -> a -> a
(%.) = flip scale ; {-# INLINE (%.) #-}


instance VectorSpace Int where
  type Scalar Int = Int  
  scale = mul ; {-# INLINE scale #-}

instance VectorSpace Integer where
  type Scalar Integer = Integer
  scale = mul ; {-# INLINE scale #-}

instance VectorSpace Float where
  type Scalar Float = Float
  scale = mul ; {-# INLINE scale #-}

instance VectorSpace Double where
  type Scalar Double = Double
  scale = mul ; {-# INLINE scale #-}



-- An inner product space
class VectorSpace a => InnerSpace a where
  dot :: a -> a -> Scalar a

(*.*) :: (InnerSpace a, s ~ Scalar a) => a -> a -> s
(*.*) = dot ; {-# INLINE (*.*) #-}


instance InnerSpace Int where
  dot = mul ; {-# INLINE dot #-}

instance InnerSpace Integer where
  dot = mul ; {-# INLINE dot #-}

instance InnerSpace Float where
  dot = mul ; {-# INLINE dot #-}

instance InnerSpace Double where
  dot = mul ; {-# INLINE dot #-}




normSq :: (InnerSpace a, Scalar a ~ s) => a -> s
normSq x = dot x x ; {-# INLINE normSq #-}

norm :: (InnerSpace a, s ~ Scalar a, Floating s) => a -> s
norm = sqrt . normSq ; {-# INLINE norm #-}

distSq :: InnerSpace a => a -> a -> Scalar a
distSq a b = normSq $ a .- b ; {-# INLINE distSq #-}

dist :: (InnerSpace a, s ~ Scalar a, Floating s) => a -> a -> s
dist a b = sqrt $ distSq a b ; {-# INLINE dist #-}

normalize' :: (InnerSpace a, s ~ Scalar a, Floating s) => a -> a
normalize' v = v `scale` (1 / norm v) ; {-# INLINE normalize' #-}

normalize :: (InnerSpace a, Epsilon s, s ~ Scalar a, Floating s) => a -> a
normalize v | closeToZero (normSq v) = error "You can not normalize a vector of length 0."
            | otherwise     = v `scale` (1 / norm v) ; {-# INLINE normalize #-}

project :: (InnerSpace v, Fractional (Scalar v)) => v -> v -> v
project u v = ((u *.* v) / normSq v) %. v ; {-# INLINE project #-}

-- 3d cross product
class HasCross v where
  cross :: v -> v -> v

-- 2d normal vector
class HasNormal v where
  normal :: v -> v











































