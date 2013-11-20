module Main where

import           Data.IORef
import           Graphics.UI.GLUT

import           Display
import           Input

import           DNA
import           ProgramState

main :: IO()
main = Main.init

init :: IO()
init = do
        dna <- newIORef (genDNA (newDNA 3) 1.0 1.0)
        mode <- newIORef 0
        level <- newIORef 3
        update <- newIORef False
        rx <- newIORef 0.0
        ry <- newIORef 0.0
        rz <- newIORef 0.0
        tx <- newIORef 0.0
        ty <- newIORef 0.0
        zoom <- newIORef 0.25
        state <- newIORef (newProgramState dna mode level update rx ry rz tx ty zoom)

        (_progName, _args) <- getArgsAndInitialize
        initialDisplayMode $= [WithDepthBuffer, DoubleBuffered, WithSamplesPerPixel 16]
        _window <- createWindow "The Code of Life"
        depthFunc $= Just Less

        reshapeCallback $= Just handleReshape
        keyboardMouseCallback $= Just (input state)
        idleCallback $= Just (animate state)
        displayCallback $= display state

        windowSize $= Size 800 800

        mainLoop
