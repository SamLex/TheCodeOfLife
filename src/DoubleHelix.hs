module DoubleHelix(DoubleHelixModel, genDHelix, renderDHelix, DoubleHelix.extractHeight) where

import           Graphics.UI.GLUT
import           Helix

type DoubleHelixModel = (HelixModel, HelixModel, GLdouble)

genDHelix :: Helix GLdouble GLdouble -> GLdouble -> DoubleHelixModel
genDHelix mhelix d = (hel 0.0 ,hel pi, Helix.extractHeight mhelix)
        where
        hel = genHelix mhelix d

renderDHelix :: DoubleHelixModel -> Int -> IO ()
renderDHelix (h1,h2, _) mode = renderHelix h1 mode >> renderHelix h2 mode

extractHeight :: DoubleHelixModel -> GLdouble
extractHeight (_,_,h) = h
