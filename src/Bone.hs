module Bones where

import Import
import Foreign.Ptr
import Foreign.C

data Joint = Joint { x :: CDouble
                   , y :: CDouble
                   , z :: CDouble
                   }

data Bone = Lig { parent :: Bone
                , mesh :: Ptr Matrix
                , color :: Ptr Matrix
                , children :: [Bone]
                , tailJoint :: Joint
                }
            | Nub { headJoint :: Joint
                  , mesh :: Ptr Matrix
                  , color :: Ptr Matrix
                  , children :: [Bone]
                  , tailJoint :: Joint
                  }

childMeshes :: Bone -> [Ptr Matrix]
childMeshes
  | Bone _ m _ [] _ = [m]
  | Bone _ m _ kids _ = m:(concat $ map childMeshes kids)
                     
