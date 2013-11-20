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
module DoubleHelix(DoubleHelixModel, genDHelix, renderDHelix, DoubleHelix.extractHeight) where

import Graphics.UI.GLUT
import Helix

type DoubleHelixModel = (HelixModel, HelixModel, GLdouble)

genDHelix :: Helix GLdouble GLdouble -> GLdouble -> DoubleHelixModel
genDHelix mhelix d = (hel 0.0 ,hel pi, Helix.extractHeight mhelix)
        where
        hel = genHelix mhelix d

renderDHelix :: DoubleHelixModel -> Int -> IO ()
renderDHelix (h1,h2, _) mode = renderHelix h1 mode >> renderHelix h2 mode

extractHeight :: DoubleHelixModel -> GLdouble
extractHeight (_,_,h) = h
