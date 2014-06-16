module Execute (
	runCommand
) where

import Parser
import Wrapper
import Sequence

import Data.Map (Map)
import qualified Data.Map as ML
import Data.Maybe
import Data.List (intersperse)
import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Foreign
import Import
import Bones

{-
test :: [Command] -> IO (RenderState ListMatrix Double)
test cs = execStateT (mapM_ runCommand cs) . genState 
	(initRenderable  (Area (0,100) (0,100)) (1,1,1))
	(Area (0,100) (0,100)) $
	1 
-}

type Tform = (Double,Double,Double)

				
getValue :: (b -> a) -> Map String b -> Val a -> a
getValue _ _ (Literal x) = x
getValue f m (Variable s) = fromMaybe (error $ "var not found: " ++ s) $
	f <$> ML.lookup s m

getTransform :: (b -> a) -> Map String b -> Transform a -> (a,a,a)
getTransform f m (a, b, c) = (getValue f m a, getValue f m b, getValue f m c)

transformApplyTri :: Ptr Matrix -> Ptr Matrix -> StateT RenderState IO ()
transformApplyTri tris cols = do
	ss@(RenderState {_currentTransform = cst,
			_currentTri = ctri,
			_colors = ccols}) <- get
	nTri <- liftIO $ extendMatrix ctri =<< applyTransform cst tris
	nCols <- liftIO $ extendMatrix ccols cols
	put $ ss { _currentTri = nTri,_colors = nCols}


--The funky bits at the beginning of each op are extracting data from the Val's, whcih are potentially variable
acol = (0,0,1)
bcol = (1,1,0)
ccol = (1,0,0)

retrieveBone :: RenderState -> Bone
retrieveBone (RenderState {_colors = col, _currentTri = ctri, _bone = bone})
	= case bone of 
		n@(Nub {}) -> n
		l@(Lig {}) -> l {color = col, mesh = ctri}

insertChild :: Bone -> Bone -> Bone
insertChild pa son = pa {children = son : children pa}

runCommand :: Command -> StateT RenderState IO ()

runCommand (Skeleton tj coms) = do
	r@(RenderState {_fnum = fnum,_varys = vs}) <- get
	let j = getTransform (seqsVal fnum) vs tj
	put $ r {_bone = Nub (toJoint j) []}
	nr <- get
	nstate <- liftIO $ execStateT (mapM_ runCommand coms) nr
	put $ nr {_bone = retrieveBone nstate}

runCommand (Bone tj coms) = do
	r@(RenderState {_fnum = fnum,_varys = vs,_bone = bone}) <- get
	let j = getTransform (seqsVal fnum) vs tj
	meshMatrix <- liftIO $ constructMatrix 0 4
	colMatrix <- liftIO $ constructMatrix 0 3
	nstate <- liftIO $ execStateT (mapM_ runCommand coms) $
		r {	_bone = Lig bone nullPtr nullPtr [] (toJoint j),
			_currentTri = meshMatrix,
			_colors = colMatrix}
	put $ r {_bone = insertChild bone $ retrieveBone nstate}

runCommand (TransformJoint Rotate tr) = do
	rs@(RenderState {_fnum = fnum,_varys = vs,
			_bone = bone,_currentTri = mesh}) <- get
	let 	(rx,ry,rz) = getTransform (seqsVal fnum) vs tr
		trans = toRad . realToFrac
		cr = (trans rx,trans ry,trans rz)
		j = jointToTform $ boneHead bone
	nBone <- liftIO $ rotateAboutHead bone cr
	liftIO $ putStrLn "ready to transform"
	liftIO $ putStrLn $ show j
	rotation <- liftIO $ rotateAboutPoint j (toRad rx,toRad ry,toRad rz)
	nMesh <- liftIO $ applyTransformFree rotation mesh
	put $ rs {_bone = nBone,_currentTri = nMesh}
	where
		toRad x = x * pi / 180
runCommand (Cube ts tr tm) = do
	RenderState {_fnum = fnum, _varys = vs} <- get
	let 
		gt = getTransform (seqsVal fnum) vs
		(s,r,m) = (gt ts,gt tr,gt tm)
	ntri <- liftIO $ cube s r m
	cols <- liftIO $ colorOfObject ntri acol bcol ccol
	transformApplyTri ntri cols


runCommand (Sphere ts tr tm) = do
	RenderState {_fnum = fnum, _varys = vs} <- get
	let 	
		gt = getTransform (seqsVal fnum) vs
		(s,r,m) = (gt ts,gt tr,gt tm)
	ntri <- liftIO $ sphere s r m
	cols <- liftIO $ colorOfObject ntri acol bcol ccol
	transformApplyTri ntri cols

runCommand (RenderCyclops te) = do
	r@(RenderState {_fnum = fnum,_varys = vs}) <- get
	let
		e = getTransform (seqsVal fnum) vs te
	liftIO $ renderState r e

runCommand (AddVar s vv vf) = do
	RenderState {_varys = vs, _fnum = fnum} <- get
	let 
		g = getValue (seqsVal fnum) vs
		f x (a,b) = (x a, x b)
		(vals,frames) = (f g vv,f (floor . g) vf)
	modify $ \ss -> ss {_varys = ML.insertWith (++) s [Anim3D frames vals] vs}

runCommand (Print s) = do
	liftIO $ putStrLn s

{-
runCommand (Transformation mode st) = do
	RenderState {_varys = vs, _fnum = fnum, _currentTransform = cst} <- get
	let s = getTransform (seqsVal fnum) vs st
	modify $ \ss -> ss {_currentTransform = transform cst $ case mode of
		Scale -> scale s
		Rotate -> rotate s
		Move -> move s}

runCommand (Save s) = do
	modify $ \ss@(RenderState {_currentTransform = cst}) ->
		ss {_transformations = ML.insert s cst $ _transformations ss}

runCommand (Restore s) = do
	modify $ \ss -> ss {_currentTransform = fromMaybe (error "transform not found") $
		ML.lookup s $ _transformations ss}


runCommand RenderParallel = do
	RenderState {_renderable = renderable,_out = out,_buffer = buf} <- get
	modify $ \ss -> ss {_buffer = foldr updateVector buf $
				renderParallel out renderable}

runCommand (RenderCyclops e) = do
	RenderState {_varys = vs, _fnum = fnum, 
		_renderable = renderable,_out = out,_buffer = buf} <- get
	let eye = getTransform (seqsVal fnum) vs e
	modify $ \ss -> ss {_buffer = foldr updateVector buf $
				renderCyclops out eye renderable}

runCommand (RenderStereo e1 e2) = do
	RenderState {_varys = vs, _fnum = fnum, _buffer= buf,
		_renderable = renderable,_out = out} <- get
	let eye1 = getTransform (seqsVal fnum) vs e1
	let eye2 = getTransform (seqsVal fnum) vs e2
	modify $ \ss -> ss {_buffer = foldr updateVector buf $
		renderStereo out (eye1,eye2) renderable}

{-
runCommand (Files s) = do
	RenderState {_fnum = fnum,
		_buffer = buf,
		_out = out} <- get
	liftIO $ writeFrame s fnum out buf
-}
-}
runCommand Unknown = return ()

--Debug
runCommand c = do
	liftIO $ putStrLn $ show c
