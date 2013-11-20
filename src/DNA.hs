module DNA (newDNA, DNAModel,renderDNA, genDNA, updateModel) where

import           Control.Monad
import           Data.IORef
import           DoubleHelix
import           Graphics.UI.GLUT
import           Helix
import           Math
import           Misc

data DNA level = DNA level

type DNAModel = [DoubleHelixModel]

renderDNA :: DNAModel -> Int -> IO ()
renderDNA d md = r d md (length d)
        where
        r _ _ 0 = return ()
        r dna mode 1 = colour (1,0,0) >> renderDHelix (head dna) mode
        r dna mode l = do
                        colour (0,1,0)
                        renderDHelix (head dna) mode
                        preservingMatrix $ do
                                rotate (-36) $ vec3f 0.0 0.0 1.0
                                forM_ (fromTo (-2*pi) (2*pi) (1/phi)) $ \(t) ->
                                        do
                                                rotate 36 $ vec3f 0.0 0.0 1.0
                                                preservingMatrix $ do
                                                        translate $ vec3f 0.0 0.0 (t * DoubleHelix.extractHeight (head dna))
                                                        rotate 36 $ vec3f 1.0 0.0 0.0
                                                        m <- newMatrix RowMajor [0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,1] :: IO (GLmatrix GLdouble)
                                                        multMatrix m
                                                        r (tail dna) mode (l-1)


genDNA :: DNA Int -> GLdouble -> GLdouble -> DNAModel
genDNA (DNA 0) _ _= []
genDNA (DNA l) h r = genDHelix (newHelix h r) 10.0 : genDNA (newDNA (l-1)) ((1/(2*pi)) * h) (0.125 * r)

newDNA :: Int -> DNA Int
newDNA = DNA

updateModel :: IORef DNAModel -> Bool -> Int -> IO ()
updateModel dna update level | update = writeIORef dna (genDNA (newDNA level) 1.0 1.0)
                             | otherwise = return ()
