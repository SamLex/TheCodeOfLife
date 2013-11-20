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
module ProgramState where

import Data.IORef
import Graphics.UI.GLUT
import DNA (DNAModel)

type ProgramStateRef = IORef ProgramState

type ProgramState = (IORef DNAModel, IORef Int, IORef Int, IORef Bool, IORef Bool, IORef GLdouble, IORef GLdouble, IORef GLdouble, IORef GLdouble, IORef GLdouble, IORef GLdouble)

newProgramState :: IORef DNAModel ->  IORef Int -> IORef Int -> IORef Bool -> IORef Bool -> IORef GLdouble -> IORef GLdouble -> IORef GLdouble -> IORef GLdouble -> IORef GLdouble -> IORef GLdouble -> ProgramState
newProgramState dna mode level update interactive rx ry rz tx ty zoom = (dna, mode, level, update, interactive, rx, ry, rz, tx, ty, zoom)

extractDNARef :: ProgramState -> IORef DNAModel
extractDNARef (dna,_,_,_,_,_,_,_,_,_,_) = dna

extractModeRef :: ProgramState -> IORef Int
extractModeRef (_,mode,_,_,_,_,_,_,_,_,_) = mode

extractLevelRef :: ProgramState -> IORef Int
extractLevelRef (_,_,level,_,_,_,_,_,_,_,_) = level

extractUpdateRef :: ProgramState -> IORef Bool
extractUpdateRef (_,_,_,update,_,_,_,_,_,_,_) = update

extractInteractiveRef :: ProgramState -> IORef Bool
extractInteractiveRef (_,_,_,_,interactive,_,_,_,_,_,_) = interactive

extractRXRef :: ProgramState -> IORef GLdouble
extractRXRef (_,_,_,_,_,rx,_,_,_,_,_) = rx

extractRYRef :: ProgramState -> IORef GLdouble
extractRYRef (_,_,_,_,_,_,ry,_,_,_,_) = ry

extractRZRef :: ProgramState -> IORef GLdouble
extractRZRef (_,_,_,_,_,_,_,rz,_,_,_) = rz

extractTXRef :: ProgramState -> IORef GLdouble
extractTXRef (_,_,_,_,_,_,_,_,tx,_,_) = tx

extractTYRef :: ProgramState -> IORef GLdouble
extractTYRef (_,_,_,_,_,_,_,_,_,ty,_) = ty

extractZoomRef :: ProgramState -> IORef GLdouble
extractZoomRef (_,_,_,_,_,_,_,_,_,_,zoom) = zoom
