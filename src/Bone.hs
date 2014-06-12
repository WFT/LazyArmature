import Import
import Foreign.Ptr

data Joint = Bone Int Int Int
data Bone = Lig Joint (Ptr Matrix) Joint

