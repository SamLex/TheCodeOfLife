module Display (display, handleReshape, animate) where

import Graphics.UI.GLUT
import Data.IORef
import Misc
import DNA

display :: IORef GLdouble -> IORef GLdouble -> IORef GLdouble -> IORef GLdouble -> IORef DNAModel -> IORef Int -> IO()
display angle zm tx tz d m = do
        clear [ColorBuffer, DepthBuffer]
        loadIdentity
        
        translate $ vec3f 0.0 0.0 (-1.0)
        
        zoom <- get zm
        thetax <- get tx
        thetaz <- get tz
        theta <- get angle
        dna <- get d
        evenScale zoom
        rotate 90.0 $ vec3f 1.0 0.0 0.0
        rotate theta $ vec3f 0.0 0.0 1.0
        rotate thetax $ vec3f 1.0 0.0 0.0
        rotate thetaz $ vec3f 0.0 0.0 1.0
--        
        renderDNA dna m
        
        -- renderObject Wireframe (Sphere' 1 100 100)
        -- renderObject Wireframe (Cylinder' 1 2 10 10)
        swapBuffers

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

animate :: IORef DNAModel -> IORef Bool -> IORef Int -> IdleCallback
animate dna update l = do
        todo <- get update
        level <- get l
        updateModel dna todo level
        writeIORef update False
        postRedisplay Nothing