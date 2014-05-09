{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LA.Matrix where


import LA.Algebra
import LA.Epsilon
import Data.Vector.Fixed
import Data.Vector.Fixed.Generic
import Prelude hiding (map, foldl, zipWith, replicate)


infix  7 *!*
infixl 7 *!
infixr 7 !*


-- Matrix x Matrix multiplication 
-------------------------------------------------------------------------------
{-# INLINE mmmulG #-}
mmmulG :: forall m u n v o w a.
          ( Additive a, Multiplicative a
          , Vector u a, Vector v a, Vector w a
          , Vector m (u a), Vector n (v a), Vector o (w a)
          , Dim m ~ Dim o, Dim u ~ Dim n, Dim v ~ Dim w
          ) => m (u a) -> n (v a) -> o (w a)
mmmulG m n = mapG (\x -> foldl (zipWithG add) (replicate zero) $ f x n) m
  where 
    f :: u a -> n (v a) -> ContVec (Dim n) (ContVec (Dim w) a)
    f = zipWithG (mapG . mul) ; {-# INLINE f #-}


{-# INLINE mmmul #-}
mmmul :: ( Additive a, Multiplicative a
        , Vector u a, Vector v a, Vector w a
        , Vector u (v a), Vector v (w a), Vector u (w a)
        ) => u (v a) -> v (w a) -> u (w a)
mmmul = mmmulG

{-# INLINE (*!*) #-}
(*!*) :: ( Additive a, Multiplicative a
         , Vector u a, Vector v a, Vector w a
         , Vector u (v a), Vector v (w a), Vector u (w a)
         ) => u (v a) -> v (w a) -> u (w a)
(*!*) = mmmulG


-- Vector x Matrix multiplication
-------------------------------------------------------------------------------
{-# INLINE vmmulG #-}
vmmulG :: forall u n v w a.
          ( Additive a, Multiplicative a
          , Vector u a, Vector v a, Vector w a
          , Vector n (v a) --, Vector n (w a)
          , Dim u ~ Dim n, Dim v ~ Dim w
          ) => u a -> n (v a) -> w a
vmmulG v m = foldl (zipWith add) (replicate zero) $ f v m
  where 
    f :: u a -> n (v a) -> ContVec (Dim n) (w a)
    f = zipWithG (mapG . mul) ; {-# INLINE f #-}

{-# INLINE vmmul #-}
vmmul :: ( Additive a, Multiplicative a
         , Vector u a, Vector v a
         , Vector u (v a)
         ) => u a -> u (v a) -> v a
vmmul = vmmulG

{-# INLINE (*!) #-}
(*!) :: ( Additive a, Multiplicative a
        , Vector u a, Vector v a
        , Vector u (v a)
        ) => u a -> u (v a) -> v a
(*!) = vmmul


-- Matrix x Vector multiplication
-------------------------------------------------------------------------------
{-# INLINE mvmulG #-}
mvmulG :: forall m u v w a.
          ( Additive a, Multiplicative a
          , Vector u a, Vector v a, Vector w a
          , Vector m (u a)
          , Dim u ~ Dim v, Dim m ~ Dim w
          ) => m (u a) -> v a -> w a 
mvmulG m v = mapG (\x -> foldl add zero $ f x v) m
  where 
    f :: ( Vector p a, Vector q a
         , Dim p ~ Dim q
         ) => p a -> q a -> q a
    f = zipWithG mul ; {-# INLINE f #-}

{-# INLINE mvmul #-}
mvmul :: ( Additive a, Multiplicative a
         , Vector u a, Vector v a
         , Vector v (u a)
         ) => v (u a) -> u a -> v a
mvmul = mvmulG


{-# INLINE (!*) #-}
(!*) :: ( Additive a, Multiplicative a
        , Vector u a, Vector v a
        , Vector v (u a)
        ) => v (u a) -> u a -> v a
(!*) = mvmul




type Matrix m n a = (Vector v a, Vector w (v a), Dim v ~ n, Dim w ~ m) => w (v a)


{-# INLINE mk11 #-}
mk11 :: a -> Matrix N1 N1 a
mk11 e1 = mk1 $ mk1 e1

{-# INLINE mk12 #-}
mk12 :: a -> a -> Matrix N1 N2 a
mk12 e1 e2 = mk1 $ mk2 e1 e2

{-# INLINE mk13 #-}
mk13 :: a -> a -> a -> Matrix N1 N3 a
mk13 e1 e2 e3 = mk1 $ mk3 e1 e2 e3

{-# INLINE mk14 #-}
mk14 :: a -> a -> a -> a -> Matrix N1 N4 a
mk14 e1 e2 e3 e4 = mk1 $ mk4 e1 e2 e3 e4

{-# INLINE mk22 #-}
mk22 :: a -> a ->
        a -> a -> Matrix N2 N2 a
mk22 e1 e2 e3 e4 = mk2 (mk2 e1 e2) (mk2 e3 e4)

{-# INLINE mk33 #-}
mk33 :: a -> a -> a ->
        a -> a -> a ->
        a -> a -> a -> Matrix N3 N3 a
mk33 e1 e2 e3
     e4 e5 e6
     e7 e8 e9 = mk3 (mk3 e1 e2 e3) (mk3 e4 e5 e6) (mk3 e7 e8 e9)

{-# INLINE mk44 #-}
mk44 :: a -> a -> a -> a ->
        a -> a -> a -> a ->
        a -> a -> a -> a -> 
        a -> a -> a -> a -> Matrix N4 N4 a
mk44 e1  e2  e3  e4
     e5  e6  e7  e8
     e9  e10 e11 e12
     e13 e14 e15 e16 = mk4 (mk4 e1  e2  e3  e4 ) (mk4 e5  e6  e7  e8 )
                           (mk4 e9  e10 e11 e12) (mk4 e13 e14 e15 e16)
