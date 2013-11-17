module Display (display, handleReshape, animate) where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad

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
        
        translate $ vec3f 0.0 0.0 (-1.0)
        
        zoom <- get zm
        thetax <- get tx
        thetaz <- get tz
        theta <- get angle
        evenScale zoom
        rotate 90.0 $ vec3f 1.0 0.0 0.0
        rotate theta $ vec3f 0.0 0.0 1.0
        rotate thetax $ vec3f 1.0 0.0 0.0
        rotate thetaz $ vec3f 0.0 0.0 1.0
                
        preservingMatrix $ do
                colour (1,0,0)
                renderPrimitive LineStrip $ mapM_ ver3d helix
        preservingMatrix $ do
                colour (0,0,1)
                rotate (-180.0) $ vec3f 0.0 0.0 1.0
                renderPrimitive LineStrip $ mapM_ ver3d helix
        
        preservingMatrix $
                forM_ (fromTo (-2*pi) (2*pi) (1/phi)) $ \(t) ->
                        renderPrimitive Lines $ preservingMatrix $ do
                                        colour (1,0,0)
                                        ver3d (helixf t t)
                                        colour (0,0,1)
                                        ver3d (helixf (t-pi) t)
                                        
--        renderPrimitive Lines $ mapM_ ver3d [(0,0,-pi), (0,0,pi)]
--        renderPrimitive Lines $ mapM_ ver3d [(-0.5,0,0), (0.5,0,0)]
--
--        preservingMatrix $ do        
--                rotate 90.0 $ vec3f 0.0 1.0 0.0
--                renderPrimitive Points $ mapM_ ver3d [(0.125 * 0.5 * cos t, 0.125 * 0.5 * sin t, (1/(4*pi)) * t) | t <- fromTo (-2*pi) (2*pi) (0.1/phi)]
        
        forM_ (fromTo (-2*pi) (2*pi) (1/phi)) $ \(t1) ->
                preservingMatrix $ do
                        translate $ vec3f 0.0 0.0 (0.5 * t1)
                        renderPrimitive Points $ mapM_ ver3d [( (1/(4*pi)) * t,0.125 * 0.5 * sin t,0.125 * 0.5 * cos t) | t <- fromTo (-2*pi) (2*pi) (0.1/phi)]
        
        swapBuffers

helix :: [(GLdouble,GLdouble,GLdouble)]
helix = [(0.5 * cos t, 0.5 * sin t, 0.5 * t) | t <- fromTo (-2*pi) (2*pi) (0.1/phi)]

helixf :: GLdouble -> GLdouble -> (GLdouble, GLdouble, GLdouble)
helixf t t1 = (0.5 * cos t, 0.5 * sin t, 0.5 * t1)

phi :: GLdouble
phi = (1 + sqrt 5)/2

handleReshape :: ReshapeCallback
handleReshape (Size w h) = do
        viewport $= (Position 0 0, Size w h)
        matrixMode $= Projection 
        loadIdentity
        let znear   = 0.001
            zfar    = 100.0
            fov     = 90.0
            angle   = (fov*pi)/360.0
            ycomp   = znear / (cos angle / sin angle)
            aspect  = fromIntegral w / fromIntegral h
            xcomp   = ycomp*aspect
        frustum (-xcomp) xcomp (-ycomp) ycomp znear zfar
        matrixMode $= Modelview 0
        postRedisplay Nothing

animate :: IORef GLdouble -> IdleCallback
animate angle = do
        angle $~! (+ (0.02/phi))
        postRedisplay Nothing