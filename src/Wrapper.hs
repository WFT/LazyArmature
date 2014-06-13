module Wrapper (
	RenderState (..),
	Matrix,
	cube,
	sphere,
	renderState,
	extendMatrix,
--	writePPM,
--	writeFrame
	) where

--import Import

import Text.Printf

import Foreign
import Foreign.Marshal.Array

import Bones

import Data.Map (Map)

import System.IO

import Data.List (sort)


import Sequence
import Import

type Tform = (Double,Double,Double)
type Eye = Tform

data RenderState = RenderState {_fnum :: Int, 
				_varys :: Map String [Sequence Double],
				_currentTransform :: Ptr Matrix,
				_transformations :: Map String (Ptr Matrix),
				_colors :: Ptr Matrix,
				_currentTri :: Ptr Matrix,
				_bone :: Bone
				}
				deriving Show


cube :: Tform -> Tform -> Tform -> IO (Ptr Matrix)
cube (sx,sy,sz) (rx,ry,rz) (x,y,z) = c_cube =<< 
	(newArray $ map realToFrac [sx,sy,sz,rx,ry,rz,x,y,z])
	

sphere :: Tform -> Tform -> Tform -> IO (Ptr Matrix)
sphere (sx,sy,sz) (rx,ry,rz) (x,y,z) = c_sphere =<<
	(newArray $ map realToFrac [sx,sy,sz,rx,ry,rz,x,y,z])

extendMatrix :: Ptr Matrix -> Ptr Matrix -> IO (Ptr Matrix)
extendMatrix mdest msrc = do 
	c_extendMatrix mdest msrc
	destructMatrix msrc
	return mdest


renderState :: RenderState -> Tform -> IO ()
renderState (RenderState {_currentTri = mesh, _colors = cs,
		_bone =  root}) 
		(ex,ey,ez) = do
	m <- newArray $ mesh : meshAndChildren root
	c <- newArray $ cs : colorAndChildren root
	e <- newArray $ map realToFrac [ex,ey,ez] -- Fix later
	renderSeries m e c



{-
writePPM :: String -> Resolution Int -> Vector (Color Int) -> IO ()
writePPM s out buffer = do
	writeFile s $ showPPM out maxColor buffer

writeFrame :: String -> Int -> Resolution Int -> Vector (Color Int) -> IO ()
writeFrame s fnum out buffer = do 
	writePPM (printf "%s%05d.ppm" s fnum) out buffer
-}
