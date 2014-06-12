import Import
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import Control.Concurrent

main = do
  setScreen (-10) (-10) 10 10
  initDisplay 500 500
  ambientLight 200 200 200
  oform <- newArray [3, 3, 3, 0, 0, 0, 0, 0, 0]
  --oforn <- newArray [3, 3, 3, 0, 0, 0, -1, -1, 0]
  m <- sphere oform
  --n <- cube oforn
  eye <- newArray [0, 0, 10]
  c1 <- newArray [1, 1, 0]
  c2 <- newArray [1, 0, 1]
  c3 <- newArray [1, 1, 1]
  colorm <- colorsForObject m c1 c2 c3
  rxyz <- xyzAboutPointMatrix 30 0 0 (-10) (-10) 0
  obj <- applyTransformFree rxyz m
  --colorn <- colorsForObject n c1 c2 c3
  --renderList [m, n] eye [colorm, colorn]
  render obj eye colorm
  -- spin m eye colorm 1300
  destructMatrix obj
  getLine
  closeDisplay

renderList :: [Ptr Matrix] -> Ptr CDouble -> [Ptr Matrix] -> IO ()
renderList faces eye colors = do
  facep <- newArray0 nullPtr faces
  colorp <- newArray0 nullPtr colors
  renderSeries facep eye colorp

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
  
