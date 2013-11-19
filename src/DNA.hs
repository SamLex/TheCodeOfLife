module DNA (newDNA, render) where

import Control.Monad
import Graphics.UI.GLUT
import Misc
import Helix
import Math

data DNA level = DNA level

render :: DNA Int -> GLdouble -> GLdouble -> IO ()
render (DNA 0) _ _ = colour (0.0, 0.0, 1.0)
render (DNA 1) h r = do
                                colour (1.0, 0.0, 0.0)
                                preMat $ renderHelix (newHelix h r) 1.0
                                preMat $ do
                                        rotate (-180) $ vec3f 0.0 0.0 1.0
                                        renderHelix (newHelix h r) 1.0
render (DNA len) h r = do
                                colour (0.0, 1.0, 0.0)
                                preMat $ renderHelix (newHelix h r) 10.0
                                preMat $ do
                                        rotate (-180) $ vec3f 0.0 0.0 1.0
                                        renderHelix (newHelix h r) 10.0
                                preMat $ do
                                        rotate (-36) $ vec3f 0.0 0.0 1.0
                                        forM_ n $ \(t) ->
                                                do
                                                colour (0.0, 0.0, 1.0)
                                                rotate 36 $ vec3f 0.0 0.0 1.0
                                                preMat $ do
                                                        translate $ vec3f 0.0 0.0 (t * r)
                                                        m <- newMatrix RowMajor [0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,1] :: IO (GLmatrix GLdouble)
                                                        multMatrix m
                                                        colour (0.0, 0.0, 1.0)
                                                        render (DNA (len-1)) ((1/(2*pi)) * h) (0.125 * r)
                                                        --renderHelix (newHelix (1/(2*pi)) 0.125)
                                --render (DNA (len-2)) (1/(2*pi)) 0.125
                                        
        where
        n = fromTo (-2*pi) (2*pi) (1/phi) 

newDNA :: Int -> DNA Int
newDNA = DNA