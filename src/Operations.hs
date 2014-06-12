module Wrapper (
	cube,
	sphere,
	renderParallel,
	renderCyclops,
	renderStereo,
--	writePPM,
--	writeFrame
	) where

import Import

import Text.Printf


import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V

import System.IO

import Data.List (sort)

type Tform = (Double,Double,Double)
type Eye = Tform


cull eye = filter (isBackface eye . rows)
projCull eye@(ex,ey,ez) = project eye . cull [ex,ey,ez]

projectGeometry :: (Matrix m) => Tform -> Renderable m Double d -> Renderable m Double d
projectGeometry eye buffer@
	(Renderable {_lineMatrix = mls,
	_triangleMatrix = mtri}) 
	= buffer {
--		_col = green, 
		_lineMatrix = project eye mls,
		_triangleMatrix = projCull eye mtri
	}

cube :: Tform -> Tform -> Tform -> IO (Ptr ())
cube (sx,sy,sz) (rx,ry,rz) (x,y,z) = fmap c_cube $ newArray [sx,sy,sz,rx,ry,rz,x,y,z]

sphere :: Tform -> Tform
sphere (sx,sy,sz) (rx,ry,rz) (x,y,z) = fmap c_sphere $ newArray [sx,sy,sz,rx,ry,rz,x,y,z]

extendMatrix :: Ptr () -> Ptr () -> IO (Ptr ())
extendMatrix mdest msrc = do 
	c_extendMatrix mdest msrc
	destructMatrix msrc
	return mdest

sphere :: (Matrix m) => Double -> Double -> Tform -> Tform -> Tform -> [m Double]
sphere rad divs s r m = (flip map) (sphereTri rad (floor divs)) 
	$ transform (collate [scale s, rotate r, move m])

renderParallel :: (Matrix m,V.Storable d) => Resolution Int -> Renderable m Double d -> [(Vector Int,Vector d)]
renderParallel out buffer@(Renderable scr col mls mtri) =
	renderVector out $ buffer {
		_triangleMatrix = filter (parallelBackface . rows) mtri
	}

renderCyclops :: (Matrix m,V.Storable d) => Resolution Int -> Eye -> Renderable m Double d -> [(Vector Int,Vector d)]
renderCyclops out eye buffer = 
	renderVector out $ projectGeometry eye buffer

renderStereo :: (Matrix m,V.Storable d) => Resolution Int -> (Eye,Eye) -> Renderable m Double d -> [(Vector Int,V.Vector d)]
renderStereo out (e1,e2) buffer = 
	(renderVector out $ projectGeometry e1 $ buffer)
		++ (renderVector out $ projectGeometry e2 $ buffer)
{-
writePPM :: String -> Resolution Int -> Vector (Color Int) -> IO ()
writePPM s out buffer = do
	writeFile s $ showPPM out maxColor buffer

writeFrame :: String -> Int -> Resolution Int -> Vector (Color Int) -> IO ()
writeFrame s fnum out buffer = do 
	writePPM (printf "%s%05d.ppm" s fnum) out buffer
-}
