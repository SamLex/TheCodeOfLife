module Helix (newHelix, renderHelix) where

import Graphics.UI.GLUT
import Math
import Misc

data Helix height radius = Helix height radius

type HelixType = Helix GLdouble GLdouble

genHelix :: HelixType -> [(GLdouble, GLdouble, GLdouble)]
genHelix (Helix height radius) = [(radius * cos t, radius * sin t, height * t) | t <- fromTo (-2*pi) (2*pi) (phi/10)]

renderHelix :: HelixType -> IO ()
renderHelix helix = renderPrimitive LineStrip $ mapM_ ver3d (genHelix helix)

newHelix :: GLdouble -> GLdouble -> HelixType
newHelix = Helix