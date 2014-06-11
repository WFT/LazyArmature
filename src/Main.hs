{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C
import Foreign.Ptr

foreign import ccall "set_screen" setScreen :: CDouble -> CDouble -> CDouble -> CDouble -> IO ()
foreign import ccall "init_live_render" initDisplay :: CInt -> CInt -> IO CChar
foreign import ccall "mat_construct" constructMatrix :: CInt -> CInt -> IO (Ptr a)
foreign import ccall "pmat" pmat :: Ptr a -> IO ()

main = do
  setScreen (-10) (-10) 10 10
  initDisplay 300 300
  m <- constructMatrix 0 4
  pmat m
