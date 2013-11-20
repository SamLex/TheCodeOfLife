module Display (display, handleReshape, animate) where

import           Control.Monad
import           Data.IORef
import           DNA
import           Graphics.UI.GLUT
import           Misc
import           ProgramState

display :: ProgramStateRef -> IO()
display state  = do
        st <- get state

        clear [ColorBuffer, DepthBuffer]
        loadIdentity

        translate $ vec3f 0.0 0.0 (-1.0)
        rotate 90.0 $ vec3f 1.0 0.0 0.0

        dna <- get (extractDNARef st)
        mode <- get (extractModeRef st)
        rx <- get (extractRXRef st)
        ry <- get (extractRYRef st)
        rz <- get (extractRZRef st)
        tx <- get (extractTXRef st)
        ty <- get (extractTYRef st)
        zoom <- get (extractZoomRef st)

        evenScale zoom

        translate $ vec3f tx 0.0 0.0
        translate $ vec3f 0.0 1.0 ty

        rotate rx $ vec3f 1.0 0.0 0.0
        rotate ry $ vec3f 0.0 1.0 0.0
        rotate rz $ vec3f 0.0 0.0 1.0

        renderDNA dna mode

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

animate :: ProgramStateRef -> IdleCallback
animate state = do
        st <- get state
        update <- get (extractUpdateRef st)
        level <- get (extractLevelRef st)
        updateModel (extractDNARef st) update level
        writeIORef (extractUpdateRef st) False
        when update (postRedisplay Nothing)
