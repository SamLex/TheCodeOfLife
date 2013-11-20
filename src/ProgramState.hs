module ProgramState where

import Data.IORef
import Graphics.UI.GLUT
import DNA (DNAModel)

type ProgramStateRef = IORef ProgramState

type ProgramState = (IORef DNAModel, IORef Int, IORef Int, IORef Bool, IORef GLdouble, IORef GLdouble, IORef GLdouble, IORef GLdouble, IORef GLdouble, IORef GLdouble)

newProgramState :: IORef DNAModel ->  IORef Int -> IORef Int -> IORef Bool -> IORef GLdouble -> IORef GLdouble -> IORef GLdouble -> IORef GLdouble -> IORef GLdouble -> IORef GLdouble -> ProgramState
newProgramState dna mode level update rx ry rz tx ty zoom = (dna, mode, level, update, rx, ry, rz, tx, ty, zoom)

extractDNARef :: ProgramState -> IORef DNAModel
extractDNARef (dna,_,_,_,_,_,_,_,_,_) = dna

extractModeRef :: ProgramState -> IORef Int
extractModeRef (_,mode,_,_,_,_,_,_,_,_) = mode

extractLevelRef :: ProgramState -> IORef Int
extractLevelRef (_,_,level,_,_,_,_,_,_,_) = level

extractUpdateRef :: ProgramState -> IORef Bool
extractUpdateRef (_,_,_,update,_,_,_,_,_,_) = update

extractRXRef :: ProgramState -> IORef GLdouble
extractRXRef (_,_,_,_,rx,_,_,_,_,_) = rx

extractRYRef :: ProgramState -> IORef GLdouble
extractRYRef (_,_,_,_,_,ry,_,_,_,_) = ry

extractRZRef :: ProgramState -> IORef GLdouble
extractRZRef (_,_,_,_,_,_,rz,_,_,_) = rz

extractTXRef :: ProgramState -> IORef GLdouble
extractTXRef (_,_,_,_,_,_,_,tx,_,_) = tx

extractTYRef :: ProgramState -> IORef GLdouble
extractTYRef (_,_,_,_,_,_,_,_,ty,_) = ty

extractZoomRef :: ProgramState -> IORef GLdouble
extractZoomRef (_,_,_,_,_,_,_,_,_,zoom) = zoom
