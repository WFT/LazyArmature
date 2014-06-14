module Import where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array

type Matrix = ()

-- display.c/h
foreign import ccall "should_quit" checkQuit :: IO CChar
foreign import ccall "set_screen" setScreen ::
  CDouble -> CDouble -> CDouble -> CDouble -> IO ()
foreign import ccall "init_live_render" initDisplay ::
  CInt -> CInt -> IO CChar
foreign import ccall "finish_live_display" closeDisplay :: IO ()

-- render.c/h
foreign import ccall "rendercyclops" render ::
  Ptr Matrix -> Ptr CDouble -> Ptr Matrix -> IO ()
foreign import ccall "renderseries" renderSeries ::
  Ptr (Ptr Matrix) -> Ptr CDouble -> Ptr (Ptr Matrix) -> IO ()
foreign import ccall "renderppm" renderPPM ::
  CString -> IO ()
foreign import ccall "spinmat" spinMatrix ::
  CInt -> CInt -> CInt -> IO (Ptr Matrix)
foreign import ccall "set_ambient_light" ambientLight ::
  CInt -> CInt -> CInt -> IO ()

-- matrix.c/h
foreign import ccall "mat_construct" constructMatrix ::
  CInt -> CInt -> IO (Ptr Matrix)
foreign import ccall "mat_destruct" destructMatrix ::
  Ptr Matrix -> IO ()
foreign import ccall "mat_multiply" matrixMultiply ::
  Ptr Matrix -> Ptr Matrix -> IO (Ptr Matrix)
foreign import ccall "mat_add_column" addColumn ::
  Ptr Matrix -> Ptr CDouble -> IO ()
foreign import ccall "mat_get_cell" getCell ::
  Ptr Matrix -> CInt -> CInt -> CDouble -> IO ()
foreign import ccall "mat_set_cell" setCell ::
  Ptr Matrix -> CInt -> CInt -> CDouble -> IO ()
foreign import ccall "mat_extend" c_extendMatrix ::
  Ptr Matrix -> Ptr Matrix -> IO ()
foreign import ccall "pmat" pmat ::
  Ptr Matrix -> IO ()

-- transform.c/h
foreign import ccall "apply_transform_free" applyTransformFree ::
  Ptr Matrix -> Ptr Matrix -> IO (Ptr Matrix)
foreign import ccall "apply_transform" applyTransform ::
  Ptr Matrix -> Ptr Matrix -> IO (Ptr Matrix)
-- we don't know if this works yet
-- foreign import ccall "apply_transform_many" applyManyTransform ::
--   Ptr Matrix -> Ptr (Ptr Matrix) -> IO ()
-- foreign import ccall "apply_transform_many_free" applyManyTransformFree ::
--   Ptr Matrix -> Ptr (Ptr Matrix) -> IO ()
foreign import ccall "identity_mat" identityMatrix :: IO (Ptr Matrix)
foreign import ccall "move_mat" moveMatrix ::
  CDouble -> CDouble -> CDouble -> IO (Ptr Matrix)
foreign import ccall "scale_mat" scaleMatrix ::
  CDouble -> CDouble -> CDouble -> IO (Ptr Matrix)
foreign import ccall "rotate_x_mat" xRotateMatrix ::
  CDouble -> IO (Ptr Matrix)
foreign import ccall "rotate_y_mat" yRotateMatrix ::
  CDouble -> IO (Ptr Matrix)
foreign import ccall "rotate_z_mat" zRotateMatrix ::
  CDouble -> IO (Ptr Matrix)
foreign import ccall "rotate_xyz_point_mat" xyzAboutPointMatrix ::
  CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO (Ptr Matrix)

-- objects.c/h

foreign import ccall "box_t" c_cube ::
  Ptr CDouble -> IO (Ptr Matrix)
foreign import ccall "sphere_t" c_sphere ::
  Ptr CDouble -> IO (Ptr Matrix)
foreign import ccall "color_for_object" colorsForObject ::
  Ptr Matrix -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO (Ptr Matrix)
