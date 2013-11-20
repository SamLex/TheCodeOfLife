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
module Input (input) where

import Graphics.UI.GLUT
import ProgramState

input :: ProgramStateRef -> KeyboardMouseCallback
input state key Down _ _ = do
                           st <- get state
                           case key of
                                (Char 's') -> extractRXRef st $~! (+ (-1)) >> update st
                                (Char 'w') -> extractRXRef st $~! (+ 1) >> update st
                                (Char 'a') -> extractRYRef st $~! (+ (-1)) >> update st
                                (Char 'd') -> extractRYRef st $~! (+ 1) >> update st
                                (Char 'q') -> extractRZRef st $~! (+ (-1)) >> update st
                                (Char 'e') -> extractRZRef st $~! (+ 1) >> update st
                                (Char '+') -> extractZoomRef st $~! (+ 0.01) >> update st
                                (Char '-') -> extractZoomRef st $~! (\z -> if z>0 then z + (-0.01) else z) >> update st
                                (SpecialKey KeyLeft) -> extractTXRef st $~! (+ (-0.01)) >> update st
                                (SpecialKey KeyRight) -> extractTXRef st $~! (+ 0.01) >> update st
                                (SpecialKey KeyDown) -> extractTYRef st $~! (+ 0.01) >> update st
                                (SpecialKey KeyUp) -> extractTYRef st $~! (+ (-0.01)) >> update st
                                (Char '1') -> extractModeRef st $= 0 >> update st
                                (Char '2') -> extractModeRef st $= 1 >> update st
                                (Char '0') -> extractModeRef st $= 2 >> update st
                                (Char '/') -> extractLevelRef st $~! (\l -> if l>1 then l + (-1) else l) >> update st
                                (Char '*') -> extractLevelRef st $~! (+ 1) >> update st
                                (SpecialKey KeyF11) ->  fullScreenToggle
                                _ -> return ()
input _ _ _ _ _= return ()

update :: ProgramState -> IO ()
update st = extractUpdateRef st $= True
