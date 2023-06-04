module Artist where

import UdGraphic
import Test.QuickCheck
import Debug.Trace

-- Problema 1

separa :: Comanda -> [Comanda]
separa (Avança x) = [Avança x] -- Si la comanda es Avança fiquem Avança en una llista
separa (Gira x) = [Gira x] -- Si la comanda es Gira fiquem Avança en una llista
separa (c1 :#: c2) = separa c1 ++ separa c2 -- Concatenem el valor de retorn de les crides recursives dels dos costats de la Comanda composta
separa Para = [] -- Si la comanda es Para retornem llista buida


-- Problema 2

ajunta :: [Comanda] -> Comanda
ajunta = foldr (:#:) Para -- Utilitzem la funcio de 1r ordre foldr fent servir l'operador :#:
-- per concatenar totes les comandes i afegir un para al final


-- Problema 3

prop_equivalent :: Comanda -> Comanda -> Bool
prop_equivalent c1 c2 = separa c1 == separa c2 -- Comparem que el valor de retorn sigui el mateix (deriving EQ a UdGraphic)

prop_split_join :: Comanda -> Bool
prop_split_join c1 = ajunta(separa c1) == c1 -- Comparem que l'ajunta del separa sigui == a la Comanda original

prop_split :: Comanda -> Bool
prop_split c = not (any esParaComposta (separa c)) -- Comprovem si algun dels elements es un Para o una Comanda composta, en aquest cas retornem False
-- negant el valor de any, que retorna True si algun element de la llista compleix la funcio esParaComposta

esParaComposta :: Comanda -> Bool
esParaComposta (Para) = True -- Si una Comanda es Para retorna True
esParaComposta (x :#: y) = True -- Si una COmanda es composta retorna True
esParaComposta _ = False -- Qualsevol altra cosa retorna False


-- Problema 4

copia :: Int -> Comanda -> Comanda
copia 0 c = Para -- Cas en que l'usuari entri un 0 per seguretat
copia 1 c = c -- Si hem de copiar 1 cop queda la comanda
copia n c = c :#: copia (n-1) c -- Si hem de copiar n cops concatenem la comanda amb una crida recursiva de copiar
 

-- Problema 5

pentagon :: Distancia -> Comanda
pentagon x = copia 5 (Avança x :#: Gira 72)


-- Problema 6

poligon :: Distancia -> Int -> Angle -> Comanda
poligon x y z = copia y (Avança x :#: Gira z) --Fem z cops el mateix moviment per obtenir un poligon


prop_poligon_pentagon :: Comanda -> Bool
prop_poligon_pentagon c = esPentagon c (obtenirValorAvança c)
  where
    esPentagon :: Comanda -> Maybe Distancia -> Bool
    esPentagon c (Just x) = c == pentagon x --Un cop aquí, igualem la comanda que ens arriba a la funció prop_poligon_pentagon amb la comanda que ens queda de fer un pentagon amb mida x
    --la mida obtinguda a la funció obtenirValorAvança, que ens dona la mida de cada costat del poligon creat. Si aquesta comanda del poligon creat coincideix amb la del pentagon, podrem
    --afirmar per tant, que la comanda passada com a paràmetre, efectivament, genera un pentagon
    esPentagon _ Nothing = False


obtenirValorAvança :: Comanda -> Maybe Distancia
obtenirValorAvança c = case separa c of -- Separem la comanda C i la recorrem fins obtenir el primer valor d'avança
  (Avança x : _) -> Just x --Si existeix, l'obtenim i ja sortim de la funció (el volem per poder igualar una funció amb l'altra poligon i pentagon)
  _ -> Nothing



-- Problema 7


espiral :: Distancia -> Int -> Distancia -> Angle -> Comanda
espiral _ 0 _ _ = Avança 0.0
espiral a b c d = Avança a :#: Gira d :#: espiral (a+c) (b-1) c d --Per recursió, a cada iteració augmentem amb el valor designat el que avancem i restem en 1 el valor del comptador, quan aquest
--sigui 0 l'execució haurà finalitzat i tindrem la comanda que ens generarà una espiral

-- Problema 9

optimitza :: Comanda -> Comanda
optimitza c = optimitza1 (separa c) -- Cridem a una funcio auxiliar i li passem una llista de les comandes que no conte cap Para

optimitza1 :: [Comanda] -> Comanda
optimitza1 = foldr f (Para) -- Definicio amb foldr
	where 
		f v (Avança 0) = v -- Cas on tenim una Comanda i un Avança 0 al final ens quedem amb la Comanda
		f (Avança 0) (Avança 0) = Para -- Cas que la comanda sigui equivalent a Para
		f v (Avança 0 :#: c) = (v :#: c) -- Cas que hi hagi un Avança 0 al mig obviem Avança 0 i agafem la comanda d'abans i despres
		f (Avança 0) v = v -- Cas que tinguem Avança 0 i una Comanda ens quedem amb la Comanda
		f v (Gira 0) = v -- Cas on tenim una Comanda i un Gira 0 al final ens quedem amb la Comanda
		f (Gira 0) (Gira 0) = Para -- Cas que la comanda sigui equivalent a Para
		f v (Gira 0 :#: c) = (v :#: c) -- Cas que hi hagi un Gira 0 al mig obviem Gira 0 i agafem la comanda d'abans i despres
		f (Gira 0) v = v -- Cas que tinguem Gira 0 i una Comanda ens quedem amb la Comanda
		f (Avança m) (Avança n) = if n+m /= 0 then Avança (n+m) else Para -- Cas que tenim Avança n i Avança m, si la suma es /= 0 sumem els 2 Avança
		-- en un de sol, en cas contrari la comanda es equivalent a Para
		f (Avança m) (Avança n :#: v) = if n+m /= 0 then (Avança (n+m) :#: v) else v -- Cas que tenim Avança n i Avança m, si la suma es /= 0 sumem els 2 Avança
		-- en un de sol, en cas contrari agafem la Comanda que ve despres
		f (Gira m) (Gira n) = if n+m /= 0 then Gira (n+m) else Para -- Cas que tenim Gira n i Gira m, si la suma es /= 0 sumem els 2 Gira
		-- en un de sol, en cas contrari la comanda es equivalent a Para
		f (Gira m) (Gira n :#: v) = if n+m /= 0 then (Gira (n+m) :#: v) else v -- Cas que tenim Gira n i Gira m, si la suma es /= 0 sumem els 2 Gira
		-- en un de sol, en cas contrari agafem la Comanda que ve despres
		f (Avança m) v = (Avança m :#: v) -- Cas que tenim Avança i una comanda despres concatenem les dues comandes
		f (Gira m) v = (Gira m :#: v) -- Cas que tenim Gira i una comanda despres concatenem les dues comandes



-- Problema 10

triangle :: Int -> Comanda
triangle n = mes :#: f n -- Inicialitzem amb un Gira 90 i la crida recursiva                                                  
  where mes = Gira 90
        menys = Gira (-90)
        f 0 = Avança 10 -- Cas base fem Avança 10
        f n = f (n-1) :#: mes :#: f (n-1) :#: menys :#: f (n-1) :#: menys :#: f (n-1) :#: mes :#: f(n-1) -- Definicio de f recursiva on es va substituint
		-- per la mateixa funció fins que arriba a l'ultim nivell (0)


-- Problema 11

fulla :: Int -> Comanda
fulla n = f n -- Inicialitzem la crida recursiva amb el nivell
	where
		mes = Gira 45
		menys = Gira (-45)
		f 0 = Avança 10 -- Cas base fem Avança 10
		f n = g (n-1) :#: Branca (menys :#: f (n-1)) :#: Branca (mes :#: f (n-1)) :#: Branca (g(n-1) :#: f (n-1)) -- Definicio de f recursiva on es va substituint
		-- per la mateixa funció fins que arriba a l'ultim nivell (0)
		g 0 = Avança 10 -- Cas base fem Avança 10
		g n = g (n-1) :#: g (n-1) -- Crida recursiva on doblem les g

-- Problema 12

hilbert :: Int -> Comanda
hilbert n = menys :#: l n -- Inicialitzem la crida amb un Gira -90 i la crida recursiva
	where 
		mes = Gira 90
		menys = Gira (-90)
		f = Avança 10
		l 0 = Para -- Cas base fem Para
		l n = mes :#: r (n-1) :#: f :#: menys :#: l (n-1) :#: f :#: l (n-1) :#: menys :#: f :#: r (n-1) :#: mes -- En cas que no arribem a l'ultim nivell fem una crida
		-- substituint l per la definició recursiva
		r 0 = Para -- Cas base fem Para
		r n = menys :#: l (n-1) :#: f :#: mes :#: r (n-1) :#: f :#: r (n-1) :#: mes :#: f :#: l (n-1) :#: menys -- En cas que no arribem a l'ultim nivell fem una crida
		-- substituint r per la definició recursiva


-- Problema 13

fletxa :: Int -> Comanda
fletxa n = f n
    where
        mes = Gira 60 --Marquem els angles en el que desplaçarem el llapis
        menys = Gira (-90)
        f 0 = Avança 10 -- Quan s'acabin les iteracions comencem a aplicar els moviments adients, en aquest cas, per cada f, avancem 10
        f n = g (n-1) :#: mes :#: f (n-1) :#: mes :#: g (n-1) --Substituïm cada f per g+f+g
        g 0 = Avança 10 -- Quan s'acabin les iteracions comencem a aplicar els moviments adients, en aquest cas, per cada g, avancem 10
        g n = f (n-1) :#: menys :#: g (n-1) :#: menys :#: f (n-1) --Substituïm cada g per f-g-f


-- Problema 14

branca :: Int -> Comanda
branca n = g n
    where
        mes = Gira 22.5 --Marquem els angles en el que desplaçarem el llapis
        menys = Gira (-22.5)
        g 0 = Avança 10 -- Quan s'acabin les iteracions comencem a aplicar els moviments adients, en aquest cas, per cada g, avancem 10
        g n = f (n-1) :#: menys :#: Branca (Branca (g (n-1)) :#: mes :#: g (n-1)) :#: mes :#: f (n-1) :#: Branca (mes :#: f (n-1) :#: g (n-1)) :#: menys :#: g(n-1) --Substituïm cada g per f-[[g]+g]+f[+fg]-g
        f 0 = Avança 10 -- Quan s'acabin les iteracions comencem a aplicar els moviments adients, en aquest cas, per cada f, avancem 10
        f n = f (n-1) :#: f (n-1) --Substituïm cada f per ff
