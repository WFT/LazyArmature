module Bones where

import Import
import Foreign.Ptr
import Foreign.C

data Joint = Bone { x :: CDouble
                  , y :: CDouble
                  , z :: CDouble
                  }

data Bone = Lig { start :: Joint
                , mesh :: (Ptr Matrix)
                , color :: (Ptr Matrix)
                , end :: Joint
                }

