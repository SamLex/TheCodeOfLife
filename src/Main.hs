module Main where

import Graphics.UI.GLUT
import Data.IORef

import Display
import Input

main :: IO()
main = Main.init

init :: IO()
init = do
        (_progName, _args) <- getArgsAndInitialize
        initialDisplayMode $= [WithDepthBuffer, DoubleBuffered, WithSamplesPerPixel 16]
        _window <- createWindow "The Code of Life"
        reshapeCallback $= Just handleReshape
        depthFunc $= Just Less
        keyboardMouseCallback $= Just input
        angle <- newIORef 0.0
        idleCallback $= Just (animate angle)
        displayCallback $= display angle
        windowSize $= Size 400 400
        mainLoop
