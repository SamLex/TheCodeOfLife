module Helix (renderHelix, genHelix, HelixModel, Helix, newHelix, extractHeight) where

import Graphics.UI.GLUT
import Math
import Misc

data Helix height radius = Helix height radius

type HelixModel = Vert3L

genHelix :: Helix GLdouble GLdouble -> GLdouble -> GLdouble -> Vert3L
genHelix (Helix height radius) d phase = [(radius * cos (t-phase), radius * sin (t-phase), height * t) | t <- fromTo (-2*pi) (2*pi) (phi/d)]

renderHelix :: HelixModel -> IO ()
renderHelix helix = renderPrimitive LineStrip $ mapM_ ver3d helix

newHelix :: GLdouble -> GLdouble -> Helix GLdouble GLdouble
newHelix = Helix

extractHeight :: Helix GLdouble GLdouble -> GLdouble
extractHeight (Helix h _) = h