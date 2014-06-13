module Wrapper (
	RenderState (..),
	Matrix,
	cube,
	sphere,
	colorOfObject,
	renderState,
	renderList,
	spin,
	extendMatrix,
	genState,
--	writePPM,
--	writeFrame
	) where

--import Import
import System.IO

import Foreign
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import Control.Concurrent

import Data.Map (Map,fromList)
import Data.List (sort)

import Text.Printf

import Import
import Bones
import Sequence

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

genState :: Int -> IO RenderState
genState fnum = do 
	identity <- identityMatrix
	colors <- constructMatrix 0 3
	mesh <- constructMatrix 0 4
	return $ RenderState 
			fnum
			(fromList [])
			identity
			(fromList [])
			colors mesh
			(Nub (Joint 0 0 0) [])

cube :: Tform -> Tform -> Tform -> IO (Ptr Matrix)
cube (sx,sy,sz) (rx,ry,rz) (x,y,z) = c_cube =<< 
	(newArray $ map realToFrac [sx,sy,sz,rx,ry,rz,x,y,z])
	

sphere :: Tform -> Tform -> Tform -> IO (Ptr Matrix)
sphere (sx,sy,sz) (rx,ry,rz) (x,y,z) = c_sphere =<<
	(newArray $ map realToFrac [sx,sy,sz,rx,ry,rz,x,y,z])

colorOfObject :: Ptr Matrix -> Tform -> Tform -> Tform -> IO (Ptr Matrix)
colorOfObject obj a b c = do
	ac <- colArr a
	bc <- colArr b
	cc <- colArr c
	colorsForObject obj ac bc cc
	where
		colArr (r,g,b) = newArray $ map realToFrac [r,g,b]

extendMatrix :: Ptr Matrix -> Ptr Matrix -> IO (Ptr Matrix)
extendMatrix mdest msrc = do 
	c_extendMatrix mdest msrc
	destructMatrix msrc
	return mdest


renderState :: RenderState -> Tform -> IO ()
renderState (RenderState {_currentTri = mesh, _colors = cs}) 
		(ex,ey,ez) = do
	e <- newArray $ map realToFrac [ex,ey,ez] -- Fix later
	renderList [mesh] e [cs]

renderList :: [Ptr Matrix] -> Ptr CDouble -> [Ptr Matrix] -> IO ()
renderList faces eye colors = do
  facep <- newArray0 nullPtr faces
  colorp <- newArray0 nullPtr colors
  renderSeries facep eye colorp
  free facep
  free colorp

spin :: Ptr Matrix -> Ptr CDouble -> Ptr Matrix -> Int -> IO ()
spin faces eye colors delay = do
  tform <- spinMatrix 1 1 1
  dup <- tform Import.* faces
  let spinIt f = do
      render f eye colors
      quit <- checkQuit
      m <- tform Import.* f
      destructMatrix f
      threadDelay delay
      if (quit == 0) then spinIt m else return ()
    in spinIt dup
  destructMatrix tform

{-
writePPM :: String -> Resolution Int -> Vector (Color Int) -> IO ()
writePPM s out buffer = do
	writeFile s $ showPPM out maxColor buffer

writeFrame :: String -> Int -> Resolution Int -> Vector (Color Int) -> IO ()
writeFrame s fnum out buffer = do 
	writePPM (printf "%s%05d.ppm" s fnum) out buffer
-}
