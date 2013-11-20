{-
        The Code Of Life - Renders a 3D interactive model of DNA (a double helix) in a recursive manner in which the bonds of the DNA are made up of more double helices and so on. Be Warned! This program can be very tasking for you PC
        Copyright (C) 2013 Euan Hunter

        This program is free software: you can redistribute it and/or modify
        it under the terms of the GNU General Public License as published by
        the Free Software Foundation, either version 3 of the License, or
        (at your option) any later version.

        This program is distributed in the hope that it will be useful,
        but WITHOUT ANY WARRANTY; without even the implied warranty of
        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
        GNU General Public License for more details.

        You should have received a copy of the GNU General Public License
        along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
module Main where

import Data.IORef
import Data.Char
import Control.Monad
import Graphics.UI.GLUT

import Display
import Input

import DNA
import ProgramState

main :: IO()
main = Main.init

init :: IO()
init = do
        
        dna <- newIORef (genDNA (newDNA 2) 1.0 1.0)
        mode <- newIORef 0
        level <- newIORef 2
        update <- newIORef False
        interactive <- newIORef True
        rx <- newIORef 0.0
        ry <- newIORef 0.0
        rz <- newIORef 0.0
        tx <- newIORef 0.0
        ty <- newIORef 0.0
        zoom <- newIORef 0.25
        state <- newIORef (newProgramState dna mode level update interactive rx ry rz tx ty zoom)

        (_progName, _args) <- getArgsAndInitialize
        parseArgs _args interactive level dna
        
        initialDisplayMode $= [WithDepthBuffer, DoubleBuffered, WithSamplesPerPixel 16]
        _window <- createWindow "The Code of Life"
        depthFunc $= Just Less

        reshapeCallback $= Just handleReshape
        inter <- get interactive
        when inter (keyboardMouseCallback $= Just (input state))
        when inter (idleCallback $= Just (animate state))
        displayCallback $= display state

        windowSize $= Size 800 800

        mainLoop

parseArgs :: [String] -> IORef Bool -> IORef Int -> IORef DNAModel -> IO ()
parseArgs [] interactive level _ = level $= 2 >> interactive $= True
parseArgs [s:_] interactive level dna | isDigit s = level $= digitToInt s >> interactive $= False >> dna $= genDNA (newDNA (digitToInt s)) 1.0 1.0
                                      | otherwise = level $= 2 >> interactive $= True