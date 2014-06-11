import Import
import Foreign

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
  render m eye color
  destructMatrix m
  putStrLn "Any key to exit..."
  getLine

-- spin :: Ptr m -> [CDouble] -> Ptr m -> Int -> IO ()
-- spin faces eye colors delay = do
--   tform <- spinMatrix 1 1 1  
