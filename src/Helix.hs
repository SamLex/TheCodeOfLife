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
module Helix (genHelix, extractHeight, renderHelix, Helix, HelixModel, newHelix) where

import Control.Monad
import Graphics.UI.GLUT
import Math
import Misc

data Helix height radius = Helix height radius

type HelixModel = Vert3L

genHelix :: Helix GLdouble GLdouble -> GLdouble -> GLdouble -> Vert3L
genHelix (Helix height radius) d phase = [(radius * cos (t-phase), radius * sin (t-phase), height * t) | t <- fromTo (-2*pi) (2*pi) (phi/d)]

renderHelix :: HelixModel -> Int -> IO ()
renderHelix helix mode | mode == 0 = renderPrimitive LineStrip $ mapM_ ver3d helix
                       | mode == 1 = forM_ helix $ \(x,y,z) ->
                                                preservingMatrix $ do
                                                        translate $ vec3f x y z
                                                        renderObject Solid (Sphere' (distanceBetween (head helix) (head (tail helix))/2) 10 10)
                       | otherwise = forM_ helix $ \(x,y,z) ->
                                                preservingMatrix $ do
                                                        translate $ vec3f x y z
                                                        rotate (-90) $ vec3f 1.0 0.0 0.0
                                                        renderObject Solid (Teapot (distanceBetween (head helix) (head (tail helix))/2))

newHelix :: GLdouble -> GLdouble -> Helix GLdouble GLdouble
newHelix = Helix

extractHeight :: Helix GLdouble GLdouble -> GLdouble
extractHeight (Helix h _) = h
