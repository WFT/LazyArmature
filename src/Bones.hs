module Bones where

import Import
import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc

data Joint = Joint { x :: CDouble
                   , y :: CDouble
                   , z :: CDouble
                   } deriving (Show)

data Bone = Lig { parent :: Bone
                , mesh :: Ptr Matrix
                , color :: Ptr Matrix
                , children :: [Bone]
                , tailJoint :: Joint
                }
            | Nub { headJoint :: Joint
                  , children :: [Bone]
                  } deriving (Show)
{-
-- untested ...
meshAndChildren :: Bone -> [Ptr Matrix]
meshAndChildren (Lig _ m _ kids _) = m : concatMap meshAndChildren kids
meshAndChildren (Nub _ kids) = concatMap meshAndChildren kids

colorAndChildren :: Bone -> [Ptr Matrix]
colorAndChildren (Lig _ _ c kids _) = c : concatMap colorAndChildren kids
colorAndChildren (Nub _ kids) = concatMap colorAndChildren kids  

transformBoneAndChildren :: Ptr Matrix -> Bone -> IO Bone
transformBoneAndChildren t (Lig p m c k tj) = do
  nmat <- applyTransform t m
  if null k
    then return (Lig p nmat c k tj)
    else do
      kids <- mapM (transformBoneAndChildren t) k
      return (Lig p nmat c kids tj)
transformBoneAndChildren t (Nub p k) = do
	fmap (Nub p) $ mapM (transformBoneAndChildren t) k

renderBoneAndChildren :: Bone -> (CDouble, CDouble, CDouble) -> IO ()
renderBoneAndChildren b (ex, ey, ez) = do
  eye <- newArray [ex, ey, ez]
  let meshes = meshAndChildren b
      colors = colorAndChildren b
      in renderList meshes eye colors
  free eye
-}
-- ...untested

-- tested:
