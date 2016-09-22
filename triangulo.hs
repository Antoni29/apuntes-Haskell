--Dados a, b y x verificar si se forma o no un triangulo.
--Si se forma calcular
--	*El perimetro
--	*La superficie
--	*Los 3 ángulos

--	Correo del profe ----------------
--	edgar@uas.edu.mx
--------------------------------------

main::IO()

-- FUNCION PRINCIPAL ------------------------------------------------------------

main = do
  putStrLn "Solucion de un Triangulo dados los 3 lados"
  putStr "Lado a (m) = "; ladoa <- getLine
  let a = read ladoa
  putStr "Lado b (m) = "; ladob <- getLine
  let b = read ladob
  putStr "Lado c (m) = "; ladoc <- getLine
  let c = read ladoc

  if esTriangulo a b c then do
    putStr "Perimetro (m) = "; print (perimetro a b c)
    putStr "Area (m)      = "; print (area a b c)
    let anga = angulo b c a
    let angb = angulo a c b
    let angc = angulo a b c
    putStr "Angulo A (grados) = "; print anga
    putStr "Angulo B (grados) = "; print angb
    putStr "Angulo C (grados) = "; print angc
    putStr "Suma de Angulos (grados) = "; print (anga + angb + angc)
  else
    putStrLn "ERROR: Con esos datos no se forma un Triangulo."

-- FUNCIONES NIVEL INFERIOR -----------------------------------------------------

esTriangulo a b c = a+b>c && a+c>b && b+c>a

perimetro a b c = a+b+c

area a b c = sqrt(s*(s-a)*(s-b)*(s-c))
             where s = (perimetro a b c)/2

angulo a b c = grados (acos((a^2 + b^2 - c^2) / (2*a*b)))

grados a = a*180/pi

-- FIN --------------------------------------------------------------------------