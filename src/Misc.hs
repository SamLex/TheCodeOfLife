module Misc (evenScale, colour, ver3d, vec3f, Vert3L) where

import Graphics.UI.GLUT

type Vert3L = [(GLdouble,GLdouble,GLdouble)]

vec3f :: GLdouble -> GLdouble -> GLdouble -> Vector3 GLdouble
vec3f = Vector3

ver3d :: (GLdouble, GLdouble, GLdouble) -> IO ()
ver3d (x, y, z) = vertex $ Vertex3 x y z

colour :: (GLfloat, GLfloat, GLfloat) -> IO ()
colour (r, b, g) = color $ Color3 r b g

evenScale :: GLdouble -> IO ()
evenScale s = scale s s s
