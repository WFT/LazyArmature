module Import where

import Foreign.C
import Foreign.Ptr

-- display.c/h
foreign import ccall "should_quit" checkQuit :: IO CChar
foreign import ccall "set_screen" setScreen ::
  CDouble -> CDouble -> CDouble -> CDouble -> IO ()
foreign import ccall "init_live_render" initDisplay ::
  CInt -> CInt -> IO CChar
foreign import ccall "finish_live_display" closeDisplay :: IO ()

-- render.c/h
foreign import ccall "rendercyclops" render ::
  Ptr m -> Ptr CDouble -> Ptr m -> IO ()
foreign import ccall "renderseries" renderSeries ::
  Ptr pm -> Ptr CDouble -> Ptr pm -> IO ()
foreign import ccall "spinmat" spinMatrix ::
  CInt -> CInt -> CInt -> IO (Ptr m)
foreign import ccall "set_ambient_light" ambientLight ::
  CInt -> CInt -> CInt -> IO ()

-- matrix.c/h
foreign import ccall "mat_construct" constructMatrix ::
  CInt -> CInt -> IO (Ptr m)
foreign import ccall "mat_destruct" destructMatrix ::
  Ptr m -> IO ()
foreign import ccall "mat_multiply" (*) ::
  Ptr m -> Ptr m -> IO (Ptr m)
foreign import ccall "mat_add_column" addColumn ::
  Ptr m -> Ptr CDouble -> IO ()
foreign import ccall "mat_get_cell" getCell ::
  Ptr m -> CInt -> CInt -> CDouble -> IO ()
foreign import ccall "mat_set_cell" setCell ::
  Ptr m -> CInt -> CInt -> CDouble -> IO ()
foreign import ccall "pmat" pmat ::
  Ptr m -> IO ()

-- transform.c/h
foreign import ccall "apply_transform_free" applyTransformFree ::
  Ptr m -> Ptr pm -> IO ()
foreign import ccall "apply_transform" applyTransform ::
  Ptr m -> Ptr pm -> IO ()
foreign import ccall "apply_transform_many" applyManyTransform ::
  Ptr m -> Ptr pm -> IO ()
foreign import ccall "apply_transform_many_free" applyManyTransformFree ::
  Ptr m -> Ptr pm -> IO ()
foreign import ccall "identity_mat" identityMatrix :: IO (Ptr m)
foreign import ccall "move_mat" moveMatrix ::
  CDouble -> CDouble -> CDouble -> IO (Ptr m)
foreign import ccall "scale_mat" scaleMatrix ::
  CDouble -> CDouble -> CDouble -> IO (Ptr m)
foreign import ccall "rotate_x_mat" xRotateMatrix ::
  CDouble -> IO (Ptr m)
foreign import ccall "rotate_y_mat" yRotateMatrix ::
  CDouble -> IO (Ptr m)
foreign import ccall "rotate_z_mat" zRotateMatrix ::
  CDouble -> IO (Ptr m)


-- objects.c/h
foreign import ccall "box_t" cube ::
  Ptr CDouble -> IO (Ptr m)
foreign import ccall "sphere_t" sphere ::
  Ptr CDouble -> IO (Ptr m)
foreign import ccall "color_for_object" colorsForObject ::
  Ptr m -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO (Ptr m)
