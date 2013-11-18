module Display (display, handleReshape, animate) where

import Graphics.UI.GLUT
import Data.IORef
import Misc
import DNA

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
        
        render (newDNA 2) 1 1
--        preservingMatrix $ do
--                colour (1,0,0)
--                renderPrimitive LineStrip $ mapM_ ver3d helix
--        preservingMatrix $ do
--                colour (0,0,1)
--                rotate (-180.0) $ vec3f 0.0 0.0 1.0
--                renderPrimitive LineStrip $ mapM_ ver3d helix
--        
--        rotate (-(360/20*2)) $ vec3f 0.0 0.0 1.0
--        forM_ (fromTo (-2*pi) (2*pi) (1/phi)) $ \(t1) ->
--                do
--                rotate (360/20*2) $ vec3f 0.0 0.0 1.0
--                preservingMatrix $ do
--                        translate $ vec3f 0.0 0.0 t1
--                        colour (1,1,1)
--                        m <- newMatrix RowMajor [0,0,1,0
--                                                ,0,1,0,0
--                                                ,1,0,0,0
--                                                ,0,0,0,1] :: IO (GLmatrix GLdouble)
--                        multMatrix m
--                        renderPrimitive LineStrip $ mapM_ ver3d [(0.125 * cos t,0.125 * sin t,(1/(2*pi)) * t) | t <- fromTo (-2*pi) (2*pi) (phi/10)]
        
        swapBuffers

--helix :: [(GLdouble,GLdouble,GLdouble)]
--helix = [(cos t, sin t, t) | t <- fromTo (-2*pi) (2*pi) (phi/10)]

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
        --angle $~! (+ (0.02/phi))
        postRedisplay Nothing