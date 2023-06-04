execute :: Comanda -> [Ln]
execute comanda = execute' (Act (Pnt 0 0) 0) comandesNoves
  where comandesNoves = separa comanda

execute' :: Act -> [Comanda] -> [Ln]

execute' (Act puntIni angleIni) ((CanviaColor l) : resta) = Ln l puntIni puntIni : (execute' (Act puntIni angleIni) resta)

execute' (Act puntIni angleIni) ((Branca c):resta) = ((Ln negre puntIni puntIni) : (execute' (Act puntFinal angleFinal) comandesBranca)) ++ (execute' (Act puntIni angleIni) resta)
  where
	comandesBranca = separa c
	(Act puntFinal angleFinal) = fesMoviment (Act puntIni angleIni) (c)

-- Cas: avança, mostrem el progres
execute' (Act puntIni angleIni) ((Avança a): resta) = Ln negre puntIni puntFinal : execute' (Act puntFinal angleFinal) resta
 where (Act puntFinal angleFinal) = fesMoviment (Act puntIni angleIni) (Avança a)
 
-- Cas: no avança, fem progres i no mostrem
execute' (Act puntIni angleIni) (c: restacom) = execute' (Act puntFinal angleFinal) restacom
 where (Act puntFinal angleFinal) = fesMoviment (Act puntIni angleIni) c
 


-- Cas base
execute' (Act _ _) [] = []

-- FUNCIO fesMoviment
-- Donat un punt, un angle i una comanda, retorna nou punt i angle un cop aplicada la comanda
fesMoviment :: Act -> Comanda -> Act
fesMoviment (Act (Pnt x y) angle) (Gira angleNou) = Act (Pnt x y) (angle+angleNou)
fesMoviment (Act (Pnt x y) angle) (Avança dist) =
  Act (Pnt (x + dist*(cos (angle* (-pi/180)))) ((y + dist*(sin (angle* (-pi/180)))))) angle
fesMoviment act para = act
