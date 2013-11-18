module Math (fromTo, phi) where

import Graphics.UI.GLUT

fromTo :: GLdouble -> GLdouble -> GLdouble -> [GLdouble]
fromTo f t inc | f >= t = []
               | otherwise = f : fromTo (f+inc) t inc
               
phi :: GLdouble
phi = (1 + sqrt 5)/2

