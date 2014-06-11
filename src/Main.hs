{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C

foreign import ccall "set_screen" setScreen :: CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall "init_live_render" initDisplay :: CInt -> CInt -> IO CChar

main = do
  setScreen (-10) (-10) 10 10
  initDisplay 300 300
  
