{-# LANGUAGE RankNTypes, TypeFamilies, ScopedTypeVariables,
    MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module LA.Transform where


import LA.Algebra
import LA.Matrix
import Data.Vector.Fixed
import Data.Vector.Fixed.Generic
import Prelude hiding (map, zipWith, foldl, replicate)


infix  7 *^*
infixl 7 *^
infixr 7 ^*


-- Rotation Matrices

-- 2d rotation
{-# INLINE rot2 #-}
rot2 :: Floating a => a -> Matrix N2 N2 a
rot2 a = mk22 c s (-s) c -- i.e. | c -s |
                         --      | s  c |
  where
    (c,s) = (cos a, sin a)

-- 3d rotation about x, y and z axis respectively
{-# INLINE rot3X #-}
{-# INLINE rot3Y #-}
{-# INLINE rot3Z #-}
rot3X, rot3Y, rot3Z :: Floating a => a -> Matrix N3 N3 a
rot3X a = mk33 1 0    0  -- Leftmost column
               0 c    s  -- Center column
               0 (-s) c  -- Rightmost column
  where (c,s) = (cos a, sin a)
rot3Y a = mk33 c    0 s  -- L
               0    1 0  -- C
               (-s) 0 c  -- R
  where (c,s) = (cos a, sin a)
rot3Z a = mk33 c    s 0  -- L
               (-s) c 0  -- C
               0    0 1  -- R
  where (c,s) = (cos a, sin a)

-- 3d rotation about an arbitary axis. The axis in question, 'a', is
-- -- assumed to be of unit length.
{-# INLINE rot3 #-}
rot3 :: (Floating a, Vector v a, Dim v ~ N3) => v a -> a -> Matrix N3 N3 a
rot3 v a = mk33 (c+ux*ux*(1-c))    (uy*ux*(1-c)+uz*s) (uz*ux*(1-c)-uy*s)  -- Leftmost column
                (ux*uy*(1-c)-uz*s) (c+uy*uy*(1-c))    (uz*uy*(1-c)+ux*s)  -- Center column
                (ux*uz*(1-c)+uy*s) (uy*uz*(1-c)-ux*s) (c+uz*uz*(1-c))     -- Rightmost column
  where
    (c,s)         = (cos a, sin a)
    (ux,uy,uz)    = convert v



type Transform n a = ( Additive a, Multiplicative a
                     , Vector v a, Vector w a, Vector u (v a)
                     , n ~ Dim u, n ~ Dim v, n ~ Dim w
                     ) => (u (v a), w a)

{-# INLINE (*^*) #-}
(*^*) :: ( Additive a, Multiplicative a
         , Vector u a, Vector v a, Vector w a
         , Vector u (v a), Vector u (w a), Vector w (v a)
         , Dim u ~ Dim v, Dim v ~ Dim w
         ) => (u (w a), w a) -> (w (v a), v a) -> (u (v a), v a)
(*^*) = ttmulG

{-# INLINE ttmul #-}
ttmul :: ( Additive a, Multiplicative a
         , Vector u a, Vector v a, Vector w a
         , Vector u (v a), Vector u (w a), Vector w (v a)
         , Dim u ~ Dim v, Dim v ~ Dim w
         ) => (u (w a), w a) -> (w (v a), v a) -> (u (v a), v a)
ttmul = ttmulG

{-# INLINE ttmulG #-}
ttmulG :: forall a o p q r s t u v w.
          ( Additive a, Multiplicative a
          , Vector o a, Vector p (o a), Vector q a
          , Vector r a, Vector s (r a), Vector t a
          , Vector u a, Vector v (u a), Vector w a
          , Dim o ~ Dim p, Dim p ~ Dim q
          , Dim q ~ Dim r, Dim r ~ Dim s, Dim s ~ Dim t
          , Dim t ~ Dim u, Dim u ~ Dim v, Dim v ~ Dim w
          ) => (p (o a), q a) -> (s (r a), t a) -> (v (u a), w a)
ttmulG (r1, t1) (r2, t2) = (mmmulG r1 r2, f (vmmulG t1 r2) t2)
  where
    f :: w a -> t a -> w a
    f = zipWithG add ; {-# INLINE f #-}

{-# INLINE (*^) #-}
(*^) :: ( Additive a, Multiplicative a
        , Vector u a, Vector v a, Vector u (v a)
        , Dim u ~ Dim v
        ) => u a -> (u (v a), v a) -> v a
(*^) = vtmulG

{-# INLINE vtmul #-}
vtmul :: ( Additive a, Multiplicative a
         , Vector u a, Vector v a, Vector u (v a)
         , Dim u ~ Dim v
         ) => u a -> (u (v a), v a) -> v a
vtmul = vtmulG

{-# INLINE vtmulG #-}
vtmulG :: forall a s t u v w.
         ( Additive a, Multiplicative a
         , Vector u a, Vector v a, Vector w (v a)
         , Vector s a, Vector t a, Vector w (t a)
         , Dim u ~ Dim v, Dim v ~ Dim w
         , Dim w ~ Dim s, Dim s ~ Dim t
         ) => s a -> (w (v a), u a) -> t a
vtmulG v (r, t) = f (vmmulG v r :: t a) t
  where
    f :: t a -> u a -> t a
    f = zipWithG add ; {-# INLINE f #-}


{-# INLINE (^*) #-}
(^*) :: ( Additive a, Multiplicative a
        , Vector u a, Vector v a, Vector u (v a)
        , Dim u ~ Dim v
        ) => (u (v a), v a) -> u a -> v a
(^*) = flip vtmulG

{-# INLINE tvmul #-}
tvmul :: ( Additive a, Multiplicative a
         , Vector u a, Vector v a, Vector u (v a)
         , Dim u ~ Dim v
         ) => (u (v a), v a) -> u a -> v a
tvmul = flip vtmulG

{-# INLINE tvmulG #-}
tvmulG :: forall a s t u v w.
         ( Additive a, Multiplicative a
         , Vector u a, Vector v a, Vector w (v a)
         , Vector s a, Vector t a, Vector w (t a)
         , Dim u ~ Dim v, Dim v ~ Dim w
         , Dim w ~ Dim s, Dim s ~ Dim t
         ) => (w (v a), u a) -> s a -> t a
tvmulG = flip vtmulG


{-# INLINE rotAboutG #-}
rotAboutG :: ( Additive a, Multiplicative a, Floating a
             , Vector u a, Vector v a
             , Vector r a, Vector s (r a), Vector t a
             , Dim u ~ N3, Dim v ~ N3
             , Dim r ~ N3, Dim s ~ N3, Dim t ~ N3
             ) => u a -> v a -> a -> (s (r a), t a)
rotAboutG e d a = (rot3 d a, v)
  where
    (c,s)   = (cos a, sin a)
    (x,y,z) = convert e
    (p,q,r) = convert d
    v       = mk3 ( (x.*(q.*q.+r.*r).-p.*(y.*q.+z.*r)).*(1.-c).+(y.*r.-z.*q).*s )
                  ( (y.*(p.*p.+r.*r).-q.*(x.*p.+z.*r)).*(1.-c).+(z.*p.-x.*r).*s )
                  ( (z.*(p.*p.+q.*q).-r.*(x.*p.+y.*q)).*(1.-c).+(x.*q.-y.*p).*s )

{-# INLINE rotAboutX #-}
rotAboutX :: ( Additive a, Multiplicative a, Floating a
             , Vector u a
             , Vector r a, Vector s (r a), Vector t a
             , Dim u ~ N3
             , Dim r ~ N3, Dim s ~ N3, Dim t ~ N3
             ) => u a -> a -> (s (r a), t a)
rotAboutX e a = (rot3X a, v)
  where
    (c,s)   = (cos a, sin a)
    (x,y,z) = convert e
    v       = mk3 zero
                  ( y.*(1.-c).+z.*s )
                  ( z.*(1.-c).-y.*s )


{-# INLINE rotAboutY #-}
rotAboutY :: ( Additive a, Multiplicative a, Floating a
             , Vector u a
             , Vector r a, Vector s (r a), Vector t a
             , Dim u ~ N3
             , Dim r ~ N3, Dim s ~ N3, Dim t ~ N3
             ) => u a -> a -> (s (r a), t a)
rotAboutY e a = (rot3Y a, v)
  where
    (c,s)   = (cos a, sin a)
    (x,y,z) = convert e
    v       = mk3 ( (x.-z).*(1.-c).+y.*s )
                  zero
                  ( (z.-x).*(1.-c).-y.*s )
    

{-# INLINE rotAboutZ #-}
rotAboutZ :: ( Additive a, Multiplicative a, Floating a
             , Vector u a
             , Vector r a, Vector s (r a), Vector t a
             , Dim u ~ N3
             , Dim r ~ N3, Dim s ~ N3, Dim t ~ N3
             ) => u a -> a -> (s (r a), t a)
rotAboutZ e a = (rot3Z a, v)
  where
    (c,s)   = (cos a, sin a)
    (x,y,z) = convert e
    v       = mk3 ( (x.-y).*(1.-c).-z.*s )
                  ( (y.-x).*(1.-c).+z.*s )
                  zero



