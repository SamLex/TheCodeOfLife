module Display (display, handleReshape, animate) where

import Graphics.UI.GLUT
import Data.IORef

ver3d :: (GLdouble, GLdouble, GLdouble) -> IO ()
ver3d (x, y, z) = vertex $ Vertex3 x y z

colour :: (GLfloat, GLfloat, GLfloat) -> IO ()
colour (r, b, g) = color $ Color3 r b g

vec3f :: GLdouble -> GLdouble -> GLdouble -> Vector3 GLdouble
vec3f = Vector3

fromTo :: GLdouble -> GLdouble -> GLdouble -> [GLdouble]
fromTo f t inc | f >= t = []
               | otherwise = f : fromTo (f+inc) t inc

evenScale :: GLdouble -> IO ()
evenScale s = scale s s s

display :: IORef GLdouble -> IO()
display angle = do
        clear [ColorBuffer, DepthBuffer]
        loadIdentity
        
        theta <- get angle
        evenScale 0.25
        rotate 90.0 $ vec3f 1.0 0.0 0.0
        rotate theta $ vec3f 0.0 0.0 1.0
        translate $ vec3f 0.0 0.0 0.0
        
        preservingMatrix $ do
                colour (1,0,0)
                renderPrimitive LineStrip $ mapM_ ver3d [(-0.5 * cos t, -0.5 * sin t, -0.5 * t) | t <- fromTo (-2*pi) (2*pi) 0.01]
                colour (0,1,0)
                renderPrimitive LineStrip $ mapM_ ver3d [(-0.5 * cos (t-pi/2), -0.5 * sin (t-pi/2), -0.5 * t) | t <- fromTo (-2*pi) (2*pi) 0.01]
                colour (0,0,1)
                renderPrimitive LineStrip $ mapM_ ver3d [(-0.5 * cos (t-pi), -0.5 * sin (t-pi), -0.5 * t) | t <- fromTo (-2*pi) (2*pi) 0.01]

        swapBuffers
           
handleReshape :: ReshapeCallback
handleReshape size = do
        viewport $= (Position 0 0, size) 
        postRedisplay Nothing

animate :: IORef GLdouble -> IdleCallback
animate angle = do
        angle $~! (+ 0.05)
        postRedisplay Nothing