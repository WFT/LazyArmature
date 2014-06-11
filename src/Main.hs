{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array

foreign import ccall "set_screen" setScreen ::
  CDouble -> CDouble -> CDouble -> CDouble -> IO ()
foreign import ccall "init_live_render" initDisplay ::
  CInt -> CInt -> IO CChar
foreign import ccall "rendercyclops" render ::
  Ptr a -> Ptr CDouble -> Ptr a -> IO ()
foreign import ccall "set_ambient_light" ambientLight ::
  CInt -> CInt -> CInt -> IO ()
foreign import ccall "mat_construct" constructMatrix ::
  CInt -> CInt -> IO (Ptr a)
foreign import ccall "mat_destruct" destructMatrix ::
  Ptr a -> IO ()
foreign import ccall "mat_add_column" addColumn ::
  Ptr a -> Ptr CDouble -> IO ()
foreign import ccall "mat_get_cell" getCell ::
  Ptr a -> CInt -> CInt -> CDouble -> IO ()
foreign import ccall "mat_set_cell" setCell ::
  Ptr a -> CInt -> CInt -> CDouble -> IO ()
foreign import ccall "pmat" pmat ::
  Ptr a -> IO ()
foreign import ccall "box_t" cube ::
  Ptr CDouble -> IO (Ptr a)
foreign import ccall "sphere_t" sphere ::
  Ptr CDouble -> IO (Ptr a)
foreign import ccall "color_for_object" colorsForObject ::
  Ptr a -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO (Ptr a)

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
