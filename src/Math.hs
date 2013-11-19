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