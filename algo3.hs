import Prelude hiding (foldr, map, filter, foldl)
-- practica 0
--
--1)
-- null:: [a]->Bool dada una lista vacia devuelve true, si no, no
-- head:: [a]->a dada una lista da el primer valor
-- tail:: [a]->[a] dada una lista, da la lista sin la cabeza
-- init:: [a]->[a] Devuelve la lista sin su último elemento 
-- last:: [a]->a Devuelve el último elemento de una lista. Falla con listas vacías.
-- take:: [a]->[a] Devuelve los primeros n elementos de una lista. Si n es mayor que la longitud de la lista, devuelve la lista completa.
-- drop:: [a]->[a] Elimina los primeros n elementos de la lista. Si n es mayor que la longitud de la lista, devuelve una lista vacía.
-- ++:: [a]->[a]->[a] concatena 2 listas
-- concat:: [[a]]->[a] Concatena una lista de listas en una sola lista.
-- reverse:: [a]->[a] Invierte el orden de los elementos de la lista.
-- elem:: Eq a=> a->[a] Devuelve true o false si esta el elemento en la lista 
-- length:: [a]->Int Devuelve el largo

--2)

valorAbsoluto :: Float -> Float
valorAbsoluto x = if x > 0 then x else (-x)

bisiesto:: Int->Bool
bisiesto 0 = True
bisiesto x | x<0 = False
           | otherwise = bisiesto (x-4)

factorial :: Int -> Int
factorial n = factorAux n 1 1

factorAux::Int->Int->Int->Int
factorAux n cont num | n==cont = num
                     | otherwise = factorAux n (cont+1) (num*(cont+1))


cantDivisoresPrimos::Int->Int
cantDivisoresPrimos n =largo(divisoresPrimos n 1)

largo::[Int]->Int
largo []=0
largo (x:xs) =1+( largo xs )

divisoresPrimos::Int->Int->[Int]
divisoresPrimos  num cont | num < cont = []
                          | (mod num cont) == 0 && esPrimo(cont) = cont : divisoresPrimos num (cont+1)
                          | otherwise = divisoresPrimos num (cont+1)

esPrimo::Int->Bool
esPrimo n = if largo(divisores n 1) == 2 then True else False

divisores::Int->Int->[Int]
divisores num cont | cont > num = []
                   | (mod num cont) == 0  = cont : divisores num (cont+1)
                   | otherwise = divisores num (cont+1)


--Ejercicio 5
--Dado el siguiente modelo para árboles binarios:
--data AB a = Nil | Bin (AB a) a (AB a)
--definir las siguientes funciones:
--a. vacioAB :: AB a → Bool que indica si un árbol es vacío (i.e. no tiene nodos).
--b. negacionAB :: AB Bool → AB Bool que dado un árbol de booleanos construye otro formado por la negación
--de cada uno de los nodos.
--c. productoAB :: AB Int → Int que calcula el producto de todos los nodos del árbol.

data AB a = Nil | Bin (AB a) a (AB a)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB x = False 

foldr::(a->b->b)->b->[a]->b
foldr _ z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

suma::Int->Int->Int
suma x y = x+y

foldrSum::Int->Int->Int
foldrSum x y = foldr (+) 0 (x:[y])

--foldrElem::Eq a=>a->[a]->Bool
--foldrElem num xs = foldr () []

map::(a->b)->[a]->[b]
map f [] = []
map f (x:xs) = f x : map f xs

filter::(a->Bool)->[a]->[a]
filter p []  =  []
filter p (x:xs) = if p x then x:filter p xs else filter p xs

filterfoldr::(a->Bool)->[a]->[a]
filterfoldr p = foldr (\x xs -> if p x then x:xs else xs)[] 

foldl::(b->a->b )->b->[a]->b
foldl funcion acumulador [] = acumulador
foldl funcion acumulador (x:xs) = foldl funcion (funcion acumulador x) xs

bin2dec = foldl(\ac b-> b + 2 * ac)

