primero (x,_) = x
segundo (_,x) = x

convierte x
   | segundo x == "m" = (1.0936 * primero x, "yd")
   | segundo x == "yd" = (0.9144 * primero x, "m")
   | segundo x == "kg" = (2.2046 * primero x, "lb")
   | segundo x == "lb" = (0.4535 * primero x, "kg")
   | otherwise = (primero x, "ERROR: '" ++ segundo x ++ "' unidad desconocida")


directorio = [ ("Antonio",22,"6671303301","8 de febrero",2500),
               ("Eduardo",21,"6671236548","En su casa",2300),
               ("Sebastian",21,"667154544","Para alla",2400),
               ("Pedro",25,"6671654565","Conocido",16000) ]

nombres db = [x | (x,_,_,_,_) <- db]

nombresdo db sdo = [(x,s) | (x,_,_,_,s) <- db, s >= sdo]

totalsdo db = sum [s | (_,_,_,_,s) <- db]


--------------------------------------------------------------
qs::(Ord a) => [a] -> [a]
qs [] = []
qs [] = [x]
qs (p:xs) = qs [x | x<-xs, x<p] ++ [p] ++ qs [x | x<-xs, x>=p]

--Sacar los numeros que la suma de los cubos de cada digito sea igual al numero mismo
pc = [(c,d,u) | c<-[1..9], d<-[0..9], u<-[0..9]]
pcsol = [(c,d,u) | (c,d,u) <- pc, 100*c+10*d+u == c^3+d^3+u^3]

pcsol2 = [100*c+10*d+u | (c,d,u) <- pc, 100*c+10*d+u == c^3+d^3+u^3]

forsyte = [numero | (c,d,u)<-pc, numero == sumacubos]
          where numero = 100*c+10*d+u
                sumacubos = c^3 + d^3 + u^3
--------------------------------------------------------------
pots = [ 2^x | x <- [0..32]]

digs num
  | num < 2   = [num]
  | otherwise = num `mod` 2 : digs (num `div` 2)

valor n = reverse (zip pots (digs n))
valor2 n = [a | (a,b) <- (valor n), b>0]