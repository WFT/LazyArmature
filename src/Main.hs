import Import
import Bones
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import Control.Concurrent

main = do
  setScreen (-10) (-10) 10 10
  initDisplay 500 500
  ambientLight 200 200 200
  skele <- testSkeleton
  renderBoneAndChildren skele (0, 0, 10)
  getLine
  skele <- rotateAboutHead skele (0, 0.3, 0)
  renderBoneAndChildren skele (0, 0, 10)
  print skele
  getLine
  closeDisplay

{--
main = do
  setScreen (-10) (-10) 10 10
  initDisplay 500 500
  ambientLight 200 200 200
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
--}
spin :: Ptr Matrix -> Ptr CDouble -> Ptr Matrix -> Int -> IO ()
spin faces eye colors delay = do
  tform <- spinMatrix 1 1 1
  dup <- tform `matrixMultiply` faces
  let spinIt f = do
      render f eye colors
      quit <- checkQuit
      m <- tform `matrixMultiply` f
      destructMatrix f
      threadDelay delay
      if (quit == 0) then spinIt m else return ()
    in spinIt dup
  destructMatrix tform
  
