module Input (input) where

import           Graphics.UI.GLUT
import           ProgramState

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
