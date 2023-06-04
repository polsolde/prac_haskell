module Artist where

import UdGraphic
import Test.QuickCheck
import Debug.Trace

-- Problema 1

separa :: Comanda -> [Comanda]
separa (Avança x) = [Avança x]
separa (Gira x) = [Gira x]
separa (c1 :#: c2) = separa c1 ++ separa c2
separa Para = []



-- Problema 2

ajunta :: [Comanda] -> Comanda
ajunta [] = Para
ajunta [c] = c :#: Para
ajunta (c:cs) = c :#: ajunta cs


-- Problema 3

prop_equivalent :: Comanda -> Comanda -> Bool
prop_equivalent c1 c2 = separa c1 == separa c2

prop_split_join :: Comanda -> Bool
prop_split_join c1 = ajunta(separa c1) == c1

-- prop_split :: Comanda -> Bool
-- prop_split c1 = separa c1

-- aux_split :: [Comanda] -> Bool
-- aux_split (c:cs) = c /= Para or c 
-- suma :: [Integer] -> Integer
-- suma ls = case ls of
-- 		[] -> 0
--		(x:xs) -> x + suma xs


-- Problema 4

copia :: Int -> Comanda -> Comanda
copia n c = if n <= 0 then Para else copiaAux n c
  where
    copiaAux :: Int -> Comanda -> Comanda
    copiaAux 1 com = com
    copiaAux m com = com :#: copiaAux (m-1) com

-- Problema 5

pentagon :: Distancia -> Comanda
pentagon x = copia 5 (Avança x :#: Gira 72)

-- Problema 6


data Poligon = Poligon {distancia :: Int, costat :: Int, angles :: Int}

poligon :: Distancia -> Int -> Angle -> Comanda
poligon x y z = copia y (Avança x :#: Gira z)

prop_poligon_pentagon :: Poligon -> Bool
prop_poligon_pentagon poligon = costat poligon == 5

-- Problema 7

espiral :: Distancia -> Float -> Distancia -> Angle -> Comanda
espiral _ 0 _ _ = Avança 0.0
espiral a b c d = Avança a :#: Gira d :#: espiral (a+c) (b-1) c d



-- Problema 9

optimitza4 :: Comanda -> Comanda
optimitza4 c = optimitza3 (separa c)

optimitza3 :: [Comanda] -> Comanda
optimitza3 = foldr f (Para)
	where 
		f v (Para) = v
		f v (Avança 0) = v
		f (Avança 0) (Avança 0) = Para
		f v (Avança 0 :#: c) = (v :#: c)
		f (Avança 0) v = v
		f v (Gira 0) = v
		f (Gira 0) (Gira 0) = Para
		f v (Gira 0 :#: c) = (v :#: c)
		f (Gira 0) v = v
		f (Para) (Avança n) = Avança n
		f (Para) (Gira n) = Gira n
		f (Avança m) (Avança n) = if n+m /= 0 then Avança (n+m) else Para
		f (Avança m) (Avança n :#: v) = if n+m /= 0 then (Avança (n+m) :#: v) else v
		f (Gira m) (Gira n) = if n+m /= 0 then Gira (n+m) else Para
		f (Gira m) (Gira n :#: v) = if n+m /= 0 then (Gira (n+m) :#: v) else v
		f (Avança m) v = (Avança m :#: v)
		f (Gira m) v = (Gira m :#: v)



-- Problema 10

triangle :: Int -> Comanda
triangle n = mes :#: f n
  where
    mes = Gira 90
    menys = Gira (-90)
    f 0 = Avança 10
    f n = f (n-1) :#: mes :#: f (n-1) :#: menys :#: f (n-1) :#: menys :#: f (n-1) :#: mes :#: f (n-1)



-- Problema 11

fulla :: Int -> Comanda
fulla n = f n
	where
		mes = Gira 45
		menys = Gira (-45)
		f 0 = Avança 10
		f n = g (n-1) :#: Branca (menys :#: f (n-1)) :#: Branca (mes :#: f (n-1)) :#: Branca (g(n-1) :#: f (n-1))
		g 0 = Avança 10
		g n = g (n-1) :#: g (n-1)

-- Problema 12

hilbert :: Int -> Comanda
hilbert n = menys :#: l n
	where 
		mes = Gira 90
		menys = Gira (-90)
		f = Avança 10
		l 0 = Para
		l n = mes :#: r (n-1) :#: f :#: menys :#: l (n-1) :#: f :#: l (n-1) :#: menys :#: f :#: r (n-1) :#: mes
		r 0 = Para
		r n = menys :#: l (n-1) :#: f :#: mes :#: r (n-1) :#: f :#: r (n-1) :#: mes :#: f :#: l (n-1) :#: menys

-- Problema 13

fletxa :: Int -> Comanda
fletxa = undefined

-- Problema 14

branca :: Int -> Comanda
branca = undefined



