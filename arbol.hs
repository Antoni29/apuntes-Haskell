--//----Ordenar Arbol---------
--BST (Binary Search Tree)
--//--------------------------


import Data.List

data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a)
               deriving (Show, Eq)

nuevo = Vacio

hoja x = Nodo x Vacio Vacio

arbol = Nodo 'm' (Nodo 'e' (hoja 'c')
	                       (hoja 'g'))
                 (Nodo 'p' Vacio
                 	       (Nodo 's' (hoja 'r')
                 	       	         Vacio))

contiene Vacio _ = False
contiene (Nodo v izq der) x
        | x == v = True
        | x < v = contiene izq x
        | x > v = contiene der x

inserta Vacio x = Nodo x Vacio Vacio
inserta (Nodo v izq der) x
        | v == Nodo v izq der
        | v < x = Nodo v izq (inserta der x)
        | v > x = Nodo v (inserta izq x) der

borra Vacio _ = Vacio
borra (Nodo v izq der) x
        | x == v = borraX (Nodo v izq der)
        | x < v = Nodo v (borra izq x) der
        | x > v = Nodo v izq (borra der x)

borraX (Nodo v Vacio der) = der
borraX (Nodo v izq Vacio) = izq
borraX (Nodo v izq der)   = (Nodo v2 izq der)
       where
       	  v2 = mas_izquierdo der

mas_izquierdo (Nodo v Vacio _) = v
mas_izquierdo (Nodo _ izq _) = mas_izquierdo
