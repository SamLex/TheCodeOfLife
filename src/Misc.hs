{-
        The Code Of Life - Renders a 3D interactive model of DNA (a double helix) in a recursive manner in which the bonds of the DNA are made up of more double helices and so on. Be Warned! This program can be very tasking for you PC
        Copyright (C) 2013 Euan Hunter

        This program is free software: you can redistribute it and/or modify
        it under the terms of the GNU General Public License as published by
        the Free Software Foundation, either version 3 of the License, or
        (at your option) any later version.

        This program is distributed in the hope that it will be useful,
        but WITHOUT ANY WARRANTY; without even the implied warranty of
        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
        GNU General Public License for more details.

        You should have received a copy of the GNU General Public License
        along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
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
