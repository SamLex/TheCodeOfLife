module Main where

import Graphics.UI.GLUT
import Data.IORef

import Display
import Input

import DNA

main :: IO()
main = Main.init

init :: IO()
init = do
        (_progName, _args) <- getArgsAndInitialize
        initialDisplayMode $= [WithDepthBuffer, DoubleBuffered, WithSamplesPerPixel 16]
        _window <- createWindow "The Code of Life"
        reshapeCallback $= Just handleReshape
        depthFunc $= Just Less
        zm <- newIORef 0.25
        tx <- newIORef 0.0
        tz <- newIORef 0.0
        level <- newIORef 2
        dna <- newIORef (genDNA (newDNA 4) 1.0 1.0)
        update <- newIORef False
        keyboardMouseCallback $= Just (input zm tx tz)
        angle <- newIORef 0.0
        idleCallback $= Just (animate dna update level)
        displayCallback $= display angle zm tx tz dna
        windowSize $= Size 800 800
        mainLoop
