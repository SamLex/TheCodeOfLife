module Input (input) where

import Graphics.UI.GLUT
import Data.IORef

input :: IORef GLdouble -> IORef GLdouble -> IORef GLdouble -> KeyboardMouseCallback
input zm tx tz key Down _ _ = case key of
        (Char '+') -> zm $~! (+ 0.01)
        (Char '-') -> zm $~! (+ (-0.01))
        (SpecialKey KeyLeft) -> tz $~! (+ (-10))
        (SpecialKey KeyRight) -> tz $~! (+ 10)
        (SpecialKey KeyDown) -> tx $~! (+ (-10))
        (SpecialKey KeyUp) -> tx $~! (+ 10)
        _ -> return ()
input _ _ _ _ _ _ _= return ()


