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
cantDivisoresPrimos n =largo (divisoresPrimos n 1)

largo::[Int]->Int
largo []=0
largo (x:xs) =1+( largo xs )

divisoresPrimos::Int->Int->[Int]
divisoresPrimos  num cont | num < cont = []
                          | (mod num cont) == 0 && esPrimo (cont) = cont : divisoresPrimos num (cont+1)
                          | otherwise = divisoresPrimos num (cont+1)

esPrimo::Int->Bool
esPrimo n = if largo (divisores n 1) == 2 then True else False

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


{--vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB x = False 
--}

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
filterfoldr p = foldr (\x xs -> if p x then x:xs else xs) []

foldl::(b->a->b )->b->[a]->b
foldl funcion acumulador [] = acumulador
foldl funcion acumulador (x:xs) = foldl funcion (funcion acumulador x) xs

bin2dec = foldl (\ac b-> b + 2 * ac)

{--
data AEB a = Hoja a | Bin (AEB a) a (AEB a)

miArbol=Bin (Hoja 3) 5 (Bin (Hoja 7) 8 (Hoja 1))

foldAEB::(a->b)->(b->a->b->b)->AEB a ->b
foldAEB casoHoja casoBin (Hoja e) = casoHoja e
foldAEB casoHoja casoBin (Bin izq raiz der) = casoBin (rec izq) raiz (rec der)
    where rec = foldAEB casoHoja casoBin
    
ramasAEB::AEB a ->[[a]]
ramasAEB = foldAEB (\x -> [[x]])(\recIzq raiz recDer -> map(raiz:)(recIzq++recDer))
--}

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

--6
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna num list = recr (\x xs rec -> if num == x then xs else x:rec ) [] list

--b) es primitiva porque utiliza la cola explicitamente


--7 ?????????

mapPares::(a->b->c)->[tuple(a,b)]->[tuple(a,b)]
mapPares _  [] = []


--b) es primitiva porque utiliza la cola explicitamente

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado num = recr (\x xs rec -> if num <= x then num:x:xs else x : rec) [num]



--7 ?????????
{--
mapPares::(a->b->c)->[tuple(a,b)]->[tuple(a,b)]
mapPares _  [] = []
mapPares f (x:xs) = f(x):rec xs
    where rec = mapPares f  
--}

--9

foldNat :: (Integer -> a -> a) -> a -> Integer -> a
foldNat f z n
  | n <= 0    = z
  | otherwise = f n (foldNat f z (n - 1))

potencia :: Integer -> Integer -> Integer
potencia num pot = foldNat (\_ acc -> num * acc) 1 pot

--10
genLista::a->(a->a)->Integer->[a]
genLista arranca f cant = foldNat (\_ acc -> acc ++ [f (last acc)]) [arranca] (cant-1)

--Usando genLista, denir la función desdeHasta, que dado un par de números (el primero menor que el
--segundo), devuelve una lista de números consecutivos desde el primero hasta el segundo.

desdeHasta::Integer->Integer->[Integer]
desdeHasta desde hasta = genLista desde (+1) ((hasta - desde) +1 )

--desdeHasta 3,5
--[3,4,5]

--Usando recursión explícita, denir los esquemas de recursión estructural (foldAB) y primitiva (recAB), y dar sus tipos

data AEB a = Hoja | Rama (AEB a) a (AEB a)
  deriving (Show)


foldAB :: b -> (b -> a -> b -> b) -> AEB a -> b
foldAB z f Hoja = z
foldAB z f (Rama i x d) = f (foldAB z f i) x (foldAB z f d)



--1)
--max2::Float->Float N
--normaVectorial::Float->Float->Float N
--subtract::Float->Float Y (-)Float->Float
--predecesor::Float->Float Y Float->Float
--evaluarEnCero 

--2)
{--
curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry::a->b->c->((a,b)->c)
uncurry f(x,y)= f x y 
--}
--No

--3)
{--
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)
--}


{--(El ejercicio prohíbe usar recursion explicita, a menos que se indique lo contrario)
Se define el tipo de datos ABNV a, que representa arboles binarios no vacios con elementos de tipo a:
data ABNV a = Hoja a | Uni a (ABNV a) | Bi (ABNV a) a (ABNV a)
Definimos el siguiente arbol para utilizar en ejemplos:
abnv = Bi (Uni 2 (Hoja 1)) 3 (Bi (Hoja 4) 5 (Uni 2 (Hoja 7)))

a) Definir las funciones foldABNV y recABNV, que implementan respectivamente los esquemas de recursion estructural y primitiva para el tipo ABNV a. (Este es el inciso en el que se permite utilizar recursion explicita).
b) Definir la funcion elemABNV :: Eq a => a -> ABNV a -> Bool, que indica si un elemento pertenece al arbol.
c) Definir la función reemplazarUno :: Eq a => a -> a -> ABNV a -> ABNV a que, dados dos elementos x e y, y un arbol; devuelve un arbol como el original, pero reemplazando la primera aparición de x por y 
(La primera desde la raíz, yendo de izquierda a derecha, en el orden de preorder).

Por ejemplo:
reemplazarUno 2 5 abnv => Bi (Uni 5 (Hoja 1)) 3 (Bi (Hoja 4) 5 (Uni 2 (Hoja 7))).
reemplazarUno 2 5 (Hoja 1) => Hoja 1.--}
data ABNV a = Hoja a | Uni a (ABNV a) | Bi (ABNV a) a (ABNV a)
  deriving (Show)

foldABNV::(a->b)->(a->b->b)->(b->a->b->b)->ABNV a->b
foldABNV fBase fUni fBi (Hoja raiz)       = fBase raiz
foldABNV fBase fUni fBi (Uni Raiz Arbol)  = fUni Raiz (foldABNV fBase fUni fBi Arbol)
foldABNV fBase fUni fBi (Bi Izq Raiz Der) = fBi (foldABNV fBase fUni fBi Izq) Raiz (foldABNV fBase fUni fBi Der)

recABNV::(a->b)->(a->ABNV a->b->b)->(ABNV a->b->a->ABNV a->b->b)->ABNV a->b
recABNV fBase fUni fBi (Hoja raiz)       = fBase raiz
recABNV fBase fUni fBi (Uni Raiz Arbol)  = fUni Raiz Arbol (recABNV fBase fUni fBi Arbol)
recABNV fBase fUni fBi (Bi Izq Raiz Der) = fBi Izq (recABNV fBase fUni fBi Izq) Raiz Der (recABNV fBase fUni fBi Der)

elemABNV :: Eq a => a -> ABNV a -> Bool
elemABNV num arbol =foldABNV (\elem ->num == elem)
                             (\raiz rec-> if num == elem then True else rec)
                             (\izq raiz der-> if num == elem then True else izq && der) arbol

reemplazarUno :: Eq a => a -> a -> ABNV a -> ABNV a
reemplazarUno x y arbol = recABNV(\raiz -> if x==raiz then Hoja y else Hoja raiz)
                                 (\raiz resto recResto -> if x==raiz then Uni y resto else Uni raiz recResto)
                                 (\restoIzq recIzq raiz restoDer recDer -> if x==raiz then Bi (restoIzq) raiz (restoDer) else 
                                        (if elem x restoIzq then Bi recIzq raiz restoDer else Bi restoIzq raiz recDer)
                                 )arbol