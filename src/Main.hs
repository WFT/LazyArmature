import System.IO
import System.Environment

import Control.Monad.Trans.State

import Import
import Bones
import Parser
import Execute
import Wrapper
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import Text.Parsec

main = do
  	(fname:_) <- getArgs
	contents <- readFile fname
	let comms = case parse parseContents "reading ur file" contents of
		(Right cs) -> cs
		(Left err) -> error $ show err
	setScreen (-10) (-10) 10 10
	initDisplay 500 500
	ambientLight 200 200 200
	
	initStates <- sequence $ map genState [1..100]
	test <- mapM (evalStateT (mapM_ runCommand comms)) initStates
--	state <- genState 0
--	evalStateT (mapM_ runCommand comms) $ state
	exit <- getLine
	oform <- newArray [3, 3, 3, 0, 0, 0, 0, 0, 0]
	oforn <- newArray [3, 3, 3, 0, 0, 0, -1, -1, 0]
	m <- c_sphere oform
	n <- c_cube oforn
	eye <- newArray [0, 0, 10]
	c1 <- newArray [1, 1, 0]
	c2 <- newArray [1, 0, 1]
	c3 <- newArray [1, 1, 1]
	colorm <- colorsForObject m c1 c2 c3
	colorn <- colorsForObject n c1 c2 c3
	putStrLn "renderList... enter to continue"
	renderList [m, n] eye [colorm, colorn]
	getLine
	putStrLn "render... enter to continue"
	rxyz <- xyzAboutPointMatrix 30 0 0 (-10) (-10) 0
	obj <- applyTransformFree rxyz m
	render obj eye colorm
	getLine
	putStrLn "spin... quit LazyArmature to continue"
	spin obj eye colorm 1300
	destructMatrix obj
	
	closeDisplay

  
