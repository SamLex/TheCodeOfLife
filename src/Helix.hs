module Helix (newHelix, renderHelix) where

import Graphics.UI.GLUT
import Math
import Misc

data Helix height radius = Helix height radius

type HelixType = Helix GLdouble GLdouble

genHelix :: HelixType -> GLdouble -> [(GLdouble, GLdouble, GLdouble)]
genHelix (Helix height radius) d = [(radius * cos t, radius * sin t, height * t) | t <- fromTo (-2*pi) (2*pi) (phi/d)]

renderHelix :: HelixType -> GLdouble -> IO ()
renderHelix helix d = renderPrimitive LineStrip $ mapM_ ver3d (genHelix helix d)

newHelix :: GLdouble -> GLdouble -> HelixType
newHelix = Helix