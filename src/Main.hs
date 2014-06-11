import Import
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import Control.Concurrent

main = do
  setScreen (-10) (-10) 10 10
  initDisplay 300 300
  ambientLight 200 200 200
  oform <- newArray [3, 3, 3, 0, 0, 0, 0, 0, 0]
  m <- cube oform
  eye <- newArray [0, 0, 10]
  c1 <- newArray [1, 1, 0]
  c2 <- newArray [1, 0, 1]
  c3 <- newArray [1, 1, 1]
  color <- colorsForObject m c1 c2 c3
  --render m eye color
  spin m eye color 10
  destructMatrix m
  putStrLn "Enter to exit..."
  getLine

spin :: Ptr m -> Ptr CDouble -> Ptr m -> Int -> IO ()
spin faces eye colors delay = do
  tform <- spinMatrix 1 1 1
  dup <- tform Import.* faces
  let spinIt f = do
      render f eye colors
      quit <- checkQuit
      m <- tform Import.* f
      destructMatrix f
      threadDelay delay
      print quit
      if (quit == 0) then spinIt m else return ()
    in spinIt dup
  
