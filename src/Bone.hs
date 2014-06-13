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
childMeshes (Lig _ m _ kids _) = m : concatMap childMeshes kids
childMeshes (Nub _ m _ kids _) = m : concatMap childMeshes kids

childColors :: Bone -> [Ptr Matrix]
childColors (Lig _ _ c kids _) = c : concatMap childMeshes kids
childColors (Nub _ _ c kids _) = c : concatMap childMeshes kids

                     
