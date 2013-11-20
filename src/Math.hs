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
module Math (fromTo, phi, distanceBetween) where

import Graphics.UI.GLUT

fromTo :: GLdouble -> GLdouble -> GLdouble -> [GLdouble]
fromTo f t inc | f >= t = []
               | otherwise = f : fromTo (f+inc) t inc

phi :: GLdouble
phi = (1 + sqrt 5)/2

distanceBetween :: (GLdouble, GLdouble, GLdouble) -> (GLdouble, GLdouble, GLdouble) -> GLdouble
distanceBetween (x1,y1,z1) (x2,y2,z2) = sqrt (xd * xd + yd * yd + zd * zd)
        where
        xd = x2 - x1
        yd = y2 - y1
        zd = z2 - z1
