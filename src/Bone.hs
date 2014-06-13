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

meshAndChildren :: Bone -> [Ptr Matrix]
meshAndChildren (Lig _ m _ kids _) = m : concatMap meshAndChildren kids
meshAndChildren (Nub _ m _ kids _) = m : concatMap meshAndChildren kids

colorAndChildren :: Bone -> [Ptr Matrix]
colorAndChildren (Lig _ _ c kids _) = c : concatMap colorAndChildren kids
colorAndChildren (Nub _ _ c kids _) = c : concatMap colorAndChildren kids  

-- untested
transformBoneAndChildren :: Ptr Matrix -> Bone -> IO Bone
transformBoneAndChildren t (Lig p m c k tj) = do
  nmat <- applyTransform t m
  if null k
    then return (Lig p nmat c k tj)
    else do
      kids <- mapM (transformBoneAndChildren t) k
      return (Lig p nmat c kids tj)
transformBoneAndChildren t (Nub p m c k tj) = do
  nmat <- applyTransform t m
  if null k
    then return (Nub p nmat c k tj)
    else do
      kids <- mapM (transformBoneAndChildren t) k
      return (Nub p nmat c kids tj)

