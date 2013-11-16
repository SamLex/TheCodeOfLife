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

display :: IORef GLdouble -> IORef GLdouble -> IORef GLdouble -> IORef GLdouble -> IO()
display angle zm tx tz= do
        clear [ColorBuffer, DepthBuffer]
        loadIdentity
        
        zoom <- get zm
        thetax <- get tx
        thetaz <- get tz
        theta <- get angle
        evenScale zoom
        rotate 90.0 $ vec3f 1.0 0.0 0.0
        rotate theta $ vec3f 0.0 0.0 1.0
        rotate thetax $ vec3f 1.0 0.0 0.0
        rotate thetaz $ vec3f 0.0 0.0 1.0
        translate $ vec3f 0.0 0.0 0.0
                
        preservingMatrix $ do
                colour (1,0,0)
                renderPrimitive LineStrip $ mapM_ ver3d helix
        preservingMatrix $ do
                colour (0,1,0)
                rotate (-90.0) $ vec3f 0.0 0.0 1.0
                renderPrimitive LineStrip $ mapM_ ver3d helix
        preservingMatrix $ do
                colour (0,0,1)
                rotate (-180.0) $ vec3f 0.0 0.0 1.0
                renderPrimitive LineStrip $ mapM_ ver3d helix
                
        swapBuffers

helix :: [(GLdouble,GLdouble,GLdouble)]
helix = [(-0.5 * cos t, -0.5 * sin t, -0.5 * t) | t <- fromTo (-2*pi) (2*pi) (0.1/phi)]
 
phi :: GLdouble
phi = (1 + sqrt 5)/2

handleReshape :: ReshapeCallback
handleReshape size = do
        viewport $= (Position 0 0, size) 
        postRedisplay Nothing

animate :: IORef GLdouble -> IdleCallback
animate angle = do
        angle $~! (+ (0.02/phi))
        postRedisplay Nothing