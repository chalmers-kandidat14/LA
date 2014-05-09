
module LA.Epsilon where


class Epsilon e where
    closeToZero :: e -> Bool

instance Epsilon Int where
    closeToZero = (==0) ; {-# INLINE closeToZero #-}

instance Epsilon Integer where
    closeToZero = (==0) ; {-# INLINE closeToZero #-}

instance Epsilon Float where
    closeToZero = (<1e-6) . abs ; {-# INLINE closeToZero #-}

instance Epsilon Double where
    closeToZero = (<1e-12) . abs ; {-# INLINE closeToZero #-}







