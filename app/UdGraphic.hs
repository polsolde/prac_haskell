module UdGraphic (
    Comanda(..),
    Distancia,
    Angle,
    execute,
    display
    )
    where


import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT hiding (Angle)
import Data.IORef
import Data.List
import Control.Monad( liftM, liftM2, liftM3 )
import System.Random
import Test.QuickCheck


infixr 5 :#:


-- Punts


data Pnt = Pnt Float Float
  deriving (Eq,Ord,Show)


instance Num Pnt where
  Pnt x y + Pnt x' y'  =  Pnt (x+x') (y+y')
  Pnt x y - Pnt x' y'  =  Pnt (x-x') (y-y')
  Pnt x y * Pnt x' y'  =  Pnt (x*x') (y*y')
  fromInteger          =  scalar . fromInteger
  abs (Pnt x y)        =  Pnt (abs x) (abs y)
  signum (Pnt x y)     =  Pnt (signum x) (signum y)


instance Fractional Pnt where
  Pnt x y / Pnt x' y'  =  Pnt (x/x') (y/y')
  fromRational         =  scalar . fromRational


scalar :: Float -> Pnt
scalar x  =  Pnt x x


scalarMin :: Pnt -> Pnt
scalarMin (Pnt x y)  =  scalar (x `min` y)


scalarMax :: Pnt -> Pnt
scalarMax (Pnt x y)  =  scalar (x `max` y)


dimensions :: Pnt -> (Int,Int)
dimensions (Pnt x y)  =  (ceiling x, ceiling y)


lub :: Pnt -> Pnt -> Pnt
Pnt x y `lub` Pnt x' y'  =  Pnt (x `max` x') (y `max` y')


glb :: Pnt -> Pnt -> Pnt
Pnt x y `glb` Pnt x' y'  =  Pnt (x `min` x') (y `min` y')


pointToSize :: Pnt -> Size
pointToSize (Pnt x y) = Size (ceiling x) (ceiling y)


sizeToPoint :: Size -> Pnt
sizeToPoint (Size x y) = Pnt (fromIntegral x) (fromIntegral y)


-- Colors


data Llapis = Color' GL.GLfloat GL.GLfloat GL.GLfloat
            | Transparent
            deriving (Eq, Ord, Show)


pencilToRGB :: Llapis -> GL.Color3 GL.GLfloat
pencilToRGB (Color' r g b)  =  GL.Color3 r g b
pencilToRGB Transparent  =  error "pencilToRGB: transparent"


blanc, negre, vermell, verd, blau :: Llapis
blanc   = Color' 1.0 1.0 1.0
negre   = Color' 0.0 0.0 0.0
vermell = Color' 1.0 0.0 0.0
verd    = Color' 0.0 1.0 0.0
blau    = Color' 0.0 0.0 1.0


-- Lines


data Ln = Ln Llapis Pnt Pnt
  deriving (Eq,Ord,Show)




-- Window parameters


theCanvas :: Pnt
theCanvas  =  Pnt 800 800


theBGcolor :: GL.Color3 GL.GLfloat
theBGcolor = pencilToRGB blanc






-- Main drawing and window functions


display :: Comanda -> IO ()
display c = do
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize  $= pointToSize theCanvas
  getArgsAndInitialize
  w <- createWindow "pencilcil Graphics"
  displayCallback $= draw c
  reshapeCallback $= Just (\x -> (viewport $= (Position 0 0, x)))
  --actionOnWindowClose $= ContinueExectuion
  draw c
  mainLoop


draw :: Comanda -> IO ()
draw c = do clear [ColorBuffer]
            loadIdentity
            background
            toGraphic $ rescale $ execute c
            swapBuffers


toGraphic :: [Ln] -> IO ()
toGraphic lines  = sequence_ (map f lines)
  where
  f (Ln pencil startP endP)  =
    GL.color (pencilToRGB pencil) >>
    GL.renderPrimitive GL.LineStrip (toVertex startP >> toVertex endP)


background :: IO ()
background = do GL.color theBGcolor
                GL.renderPrimitive GL.Polygon $ mapM_ GL.vertex
                      [GL.Vertex3 (-1) (-1) 0,
                       GL.Vertex3   1  (-1) 0,
                       GL.Vertex3   1    1  0,
                       GL.Vertex3 (-1)   1 (0::GL.GLfloat) ]




toVertex (Pnt x y)  =  GL.vertex $ GL.Vertex3
 (realToFrac x) (realToFrac y) (0::GL.GLfloat)






-- Definició de les comandes per moure el llapis


type Angle     = Float
type Distancia = Float
data Comanda   = Avança Distancia
               | Gira Angle
               | Comanda :#: Comanda
               | Para
               | Branca Comanda
               deriving (Show, Eq)






separa :: Comanda -> [Comanda]
separa (Avança x) = [Avança x] -- Si la comanda es Avança fiquem Avança en una llista
separa (Gira x) = [Gira x] -- Si la comanda es Gira fiquem Avança en una llista
separa (c1 :#: c2) = separa c1 ++ separa c2 -- Concatenem el valor de retorn de les crides recursives dels dos costats de la Comanda composta
separa Para = [] -- Si la comanda es Para retornem llista buida




-- Problema 8
-- Pas de comandes a lines a pintar per GL graphics


-- Problema 8
--data Color = Color Double Double Double
--data Punt = Pnt Double Double
--data Linia = Ln Color Punt Punt
 
 
execute :: Comanda -> [Ln]
execute c = aux (separa c) (Pnt 0 0) 0 -- passem la llista de comandes, el punt origen i l'angle 0
aux :: [Comanda] -> Pnt -> Angle -> [Ln]
aux ((Avança x):xs) pIni angle = Ln (Color' 0.0 0.0 0.0) (pIni) (pFin) : aux xs pFin angleFin -- concatenem la linia de la comanda x amb la crida recursiva
-- canviant el punt inicial
  where
    pFin = (mou pIni x angle) -- busquem el punt final
    angleFin = angle -- l'angle es queda igual
aux ((Gira x):xs) pIni angle = Ln (Color' 0.0 0.0 0.0) pIni pFin : aux xs pFin (angle + x) -- concatenem la linia de la comanda x amb la crida recursiva
-- canviant l'angle
  where
    pFin = pIni -- es queda igual
aux [] _ _ = [] -- cas base


mou :: Pnt -> Distancia -> Angle -> Pnt
mou (Pnt x y) dist angle = Pnt (x + dist*(cos (angle* (-pi/180)))) ((y + dist*(sin (angle* (-pi/180))))) -- trobem el punt desti a traves de calculs





-- Rescales all points in a list of lines
--  from an arbitrary scale
--  to (-1.-1) - (1.1)


rescale :: [Ln] -> [Ln]
rescale lines | points == [] = []
              | otherwise    = map f lines
  where
  f (Ln pencil p q)  =  Ln pencil (g p) (g q)
  g p             =  swap ((p - p0) / s)
  points          =  [ r | Ln pencil p q <- lines, r <- [p, q] ]
  hi              =  foldr1 lub points
  lo              =  foldr1 glb points
  s               =  scalarMax (hi - lo) * scalar (0.55)
  p0              =  (hi + lo) * scalar (0.5)
  swap (Pnt x y)  =  Pnt y x




-- Generators for QuickCheck


instance Arbitrary Llapis where
    arbitrary  =  sized pencil
        where
          pencil n  =  elements [negre,vermell,verd,blau,blanc,Transparent]




instance Arbitrary Comanda where
    arbitrary  =  sized cmd
        where
          cmd n  |  n <= 0     =  oneof [liftM (Avança . abs) arbitrary,
                                         liftM Gira arbitrary ]
                 |  otherwise  =  liftM2 (:#:) (cmd (n `div` 2)) (cmd (n `div`2))