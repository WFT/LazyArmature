{-# LANGUAGE ForeignFunctionInterface #-}
module Display where

import Foreign.C

foreign import ccall "set_screen" setScreen :: CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall "init_live_render" initDisplay :: CInt -> CInt -> IO CChar
