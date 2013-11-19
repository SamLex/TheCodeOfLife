module Helix (renderHelix, genHelix, HelixModel, Helix, newHelix, extractHeight, extractRadius) where

import Graphics.UI.GLUT
import Math
import Misc
import Control.Monad

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

extractRadius :: Helix GLdouble GLdouble -> GLdouble
extractRadius (Helix _ r) = r