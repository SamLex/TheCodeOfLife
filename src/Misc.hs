module Misc where

import Graphics.UI.GLUT

preMat :: IO a -> IO a
preMat = preservingMatrix

vec3f :: GLdouble -> GLdouble -> GLdouble -> Vector3 GLdouble
vec3f = Vector3

ver3d :: (GLdouble, GLdouble, GLdouble) -> IO ()
ver3d (x, y, z) = vertex $ Vertex3 x y z

colour :: (GLfloat, GLfloat, GLfloat) -> IO ()
colour (r, b, g) = color $ Color3 r b g

evenScale :: GLdouble -> IO ()
evenScale s = scale s s s