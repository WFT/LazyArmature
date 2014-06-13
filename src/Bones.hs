module Bones where

import Import
import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc

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

-- untested ...
meshAndChildren :: Bone -> [Ptr Matrix]
meshAndChildren (Lig _ m _ kids _) = m : concatMap meshAndChildren kids
meshAndChildren (Nub _ m _ kids _) = m : concatMap meshAndChildren kids

colorAndChildren :: Bone -> [Ptr Matrix]
colorAndChildren (Lig _ _ c kids _) = c : concatMap colorAndChildren kids
colorAndChildren (Nub _ _ c kids _) = c : concatMap colorAndChildren kids  

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

renderBoneAndChildren :: Bone -> (CDouble, CDouble, CDouble) -> IO ()
renderBoneAndChildren b (ex, ey, ez) = do
  eye <- newArray [ex, ey, ez]
  let meshes = meshAndChildren b
      colors = colorAndChildren b
      in renderList meshes eye colors
  free eye

rotateAboutHead :: Bone -> (CDouble, CDouble, CDouble) -> IO Bone
rotateAboutHead b (rx, ry, rz) = do
  tform <- xyzAboutPointMatrix rx ry rz hx hy hz
  bon <- transformBoneAndChildren tform b
  free tform
  return bon
  where h = case b of (Lig p _ _ _ _) -> tailJoint p
                      (Nub hj _ _ _ _) -> hj
        hx = x h
        hy = y h
        hz = z h
                        

-- ...untested

-- tested:
renderList :: [Ptr Matrix] -> Ptr CDouble -> [Ptr Matrix] -> IO ()
renderList faces eye colors = do
  facep <- newArray0 nullPtr faces
  colorp <- newArray0 nullPtr colors
  renderSeries facep eye colorp
  free facep
  free colorp
