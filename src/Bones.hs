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

transformMeshSkeleton :: Ptr Matrix -> Bone -> IO Bone
transformMeshSkeleton t (Lig p m c k tj) = do
  nmat <- applyTransform t m
  if null k
    then return (Lig p nmat c k tj)
    else do
      kids <- mapM (transformMeshSkeleton t) k
      return (Lig p nmat c kids tj)
transformMeshSkeleton t (Nub p m c k tj) = do
  nmat <- applyTransform t m
  if null k
    then return (Nub p nmat c k tj)
    else do
      kids <- mapM (transformMeshSkeleton t) k
      return (Nub p nmat c kids tj)

-- takes a transform matrix & joint transform function
-- applies to given bone and its descendents
transformSkeleton :: Ptr Matrix -> (Joint -> Joint) -> Bone -> IO Bone
transformSkeleton t jtform (Lig p m c k tj) = do
  nmat <- applyTransform t m
  if null k
    then return (Lig p nmat c k (jtform tj))
    else do
      kids <- mapM (transformSkeleton t jtform) k
      return (Lig p nmat c kids (jtform tj))
transformSkeleton t jtform (Nub p m c k tj) = do
  nmat <- applyTransform t m
  if null k
    then return (Nub p nmat c k (jtform tj))
    else do
      kids <- mapM (transformSkeleton t jtform) k
      return (Nub p nmat c kids (jtform tj))

renderBoneAndChildren :: Bone -> (CDouble, CDouble, CDouble) -> IO ()
renderBoneAndChildren b (ex, ey, ez) = do
  eye <- newArray [ex, ey, ez]
  let meshes = meshAndChildren b
      colors = colorAndChildren b
      in renderList meshes eye colors
  free eye

rotateJointAboutX :: Joint -> CDouble -> Joint
rotateJointAboutX (Joint xi yi zi) xrad =
  let y' = (yi * (cos xrad)) - (zi * (sin xrad))
      z' = (yi * (sin xrad)) + (zi * (cos xrad))
      in Joint xi y' z'

rotateJointAboutY :: Joint -> CDouble -> Joint
rotateJointAboutY (Joint xi yi zi) yrad =
  let x' = (zi * (cos yrad)) - (xi * (sin yrad))
      z' = (zi * (sin yrad)) + (xi * (cos yrad))
      in Joint x' yi z'

rotateJointAboutZ :: Joint -> CDouble -> Joint
rotateJointAboutZ (Joint xi yi zi) zrad =
  let x' = (xi * (cos zrad)) - (yi * (sin zrad))
      y' = (xi * (sin zrad)) + (yi * (cos zrad))
      in Joint x' y' zi

rotateJointAboutOrigin :: Joint -> (CDouble, CDouble, CDouble) -> Joint
rotateJointAboutOrigin j (rx, ry, rz) = let j1 = rotateJointAboutZ j rz
                                            j2 = rotateJointAboutY j ry
                                        in rotateJointAboutX j rx

translateJoint :: Joint -> (CDouble, CDouble, CDouble) -> Joint
translateJoint (Joint jx jy jz) (mx, my, mz) = Joint (jx + mx) (jy + my) (jz + mz)

rotateJointAboutJoint ::  (CDouble, CDouble, CDouble) -> Joint -> Joint -> Joint
rotateJointAboutJoint rot (Joint jx jy jz) (Joint ox oy oz) = let j1 = translateJoint (Joint jx jy jz) (jx-ox, jy-oy, jz-oz)
                                                                  j2 = rotateJointAboutOrigin j1 rot
                                                              in translateJoint j2 (ox-jx, oy-jy, oz-jz)
                                    

rotateAboutHead :: Bone -> (CDouble, CDouble, CDouble) -> IO Bone
rotateAboutHead b (rx, ry, rz) = do
  tform <- xyzAboutPointMatrix rx ry rz hx hy hz
  bon <- transformSkeleton tform (rotateJointAboutJoint (rx, ry, rz) h) b
  free tform
  return bon
  where h = case b of (Lig p _ _ _ _) -> tailJoint p
                      (Nub hj _ _ _ _) -> hj
        hx = x h
        hy = y h
        hz = z h

testSkeleton :: IO Bone
testSkeleton = do
  ofrm <- newArray [1, 1, 1, 0, 0, 0, -2, 0, 0]
  c <- c_cube ofrm
  s <- c_sphere ofrm
  c1 <- newArray [1, 1, 0]
  c2 <- newArray [1, 0, 1]
  c3 <- newArray [1, 1, 1]
  colors1 <- colorsForObject c c1 c2 c3
  colors2 <- colorsForObject s c1 c2 c3
  let j = Joint (-3) 0 0
      j2 = Joint 3 0 0
      
      --sub = Lig 
      root = (Nub j c colors1 [] j2)
    in return root

  -- ...untested

-- tested:
renderList :: [Ptr Matrix] -> Ptr CDouble -> [Ptr Matrix] -> IO ()
renderList faces eye colors = do
  facep <- newArray0 nullPtr faces
  colorp <- newArray0 nullPtr colors
  renderSeries facep eye colorp
  free facep
  free colorp
