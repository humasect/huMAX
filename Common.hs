module Common where

import Graphics.Rendering.OpenGL.GL.BasicTypes
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.BeginEnd

type Scalar = GLdouble

width, height :: Int
width = 800
height = 600

bounds = ((fromIntegral width) / 2, (fromIntegral height) / 2)

vertices2 :: [(Scalar,Scalar)] -> [Vertex2 Scalar]
vertices2 vs = map (\(x,y) -> Vertex2 x y) vs

vertices3 :: [(Scalar,Scalar,Scalar)] -> [Vertex3 Scalar]
vertices3 vs = map (\(x,y,z) -> Vertex3 x y z) vs

translateMatrix :: V2 -> IO a -> IO a
translateMatrix (x,y) f =
  unsafePreservingMatrix $ do
    translate $ Vector3 x y 0
    f

type V3 = (Scalar,Scalar,Scalar)
type V2 = (Scalar,Scalar)

v3add :: V3 -> V3 -> V3
v3add (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)

v2add :: V2 -> V2 -> V2
v2add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

--(~$) f a b = f (b a)