
module LA ( module LA.Instances
          , module LA.Algebra
          , module LA.Epsilon
          , (~=)
          )where

import qualified LA.Instances
import LA.Epsilon
import LA.Algebra



(~=) :: (Additive a, Epsilon a) => a -> a -> Bool
(~=) a b = closeToZero $ (a .- b)




