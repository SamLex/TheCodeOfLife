module DoubleHelix(DoubleHelixModel, genDHelix, renderDHelix) where

import Graphics.UI.GLUT
import Helix

type DoubleHelixModel = (HelixModel, HelixModel, GLdouble)

genDHelix :: Helix GLdouble GLdouble -> GLdouble -> DoubleHelixModel
genDHelix mhelix d = (hel 0.0 ,hel pi, extractHeight mhelix)
        where
        hel = genHelix mhelix d
        
renderDHelix :: DoubleHelixModel -> IO ()
renderDHelix (h1,h2, _) = renderHelix h1 >> renderHelix h2
