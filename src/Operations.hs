module Operations (
	RenderState (..),
	cube,
	sphere,
	renderParallel,
	renderCyclops,
	renderStereo,
--	writePPM,
--	writeFrame
	) where

import Text.Printf

import Foreign
import Foreign.Marshal.Array

--import Bones

import Data.Map

import System.IO

import Data.List (sort)


import Sequence
--import Import

type Tform = (Double,Double,Double)
type Eye = Tform

data RenderState = RenderState {_fnum :: Int, 
				_varys :: Map String [Sequence Double],
				_currentTransform :: Ptr (),
				_transformations :: Map String Ptr,
				_colors :: Ptr (),
				_currentTri :: Ptr (),
				_bones :: Bone
				}
				deriving Show


cube :: Tform -> Tform -> Tform -> IO (Ptr ())
cube s r m = (flip map) unitCube $ transform (collate [scale s, rotate r, move m])

sphere :: Double -> Double -> Tform -> Tform -> Tform -> IO (Ptr ())
sphere rad divs s r m = (flip map) (sphereTri rad (floor divs)) 
	$ transform (collate [scale s, rotate r, move m])

addMesh :: Ptr () -> Ptr () -> IO (Ptr ())
addMesh dest src = 

renderState :: RenderState -> IO ()
renderState (RenderState {_currentTri = mesh, _colors = cs,
		_bone =  root}) = do
	m <- newArray $ mesh ++ meshAndChildren root
	c <- newArray $ cs ++ colorAndChildren root




{-
writePPM :: String -> Resolution Int -> Vector (Color Int) -> IO ()
writePPM s out buffer = do
	writeFile s $ showPPM out maxColor buffer

writeFrame :: String -> Int -> Resolution Int -> Vector (Color Int) -> IO ()
writeFrame s fnum out buffer = do 
	writePPM (printf "%s%05d.ppm" s fnum) out buffer
-}
