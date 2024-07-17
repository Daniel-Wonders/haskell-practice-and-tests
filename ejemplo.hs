import System.IO.Error (isIllegalOperationErrorType)
import System.Win32 (COORD(xPos, yPos), xBUTTON1, SECURITY_ATTRIBUTES (bInheritHandle))
doubleMe x = x + x

sumaDistintos:: Int->Int->Int->Int
sumaDistintos x y z | (x==y)&&(y==z)  =x        --If the 3 input are the same
                    | (x==y) = (x+z)            --If 2 of the inputs are the same
                    | (x==z) = x+y              -- ""
                    | (y==z) = x+z              -- ""
                    | otherwise = (x + z + y)   --If none of the inputs are the same, sum all of them


digitoUnidades:: Int->Int
digitoUnidades x|(x>=0)= mod x 10               --If it's positive, mod
                |otherwise =mod (-x) 10       --If it's negative, multiply by -1 and mod

digitoDecenas::Int->Int
digitoDecenas x | (x>=0) = mod (x- digitoUnidades x) 100       --If it's positive, 
                | otherwise = mod ((-x) - digitoUnidades x) 100

prodInt:: (Float, Float)->(Float, Float)->Float
prodInt (x,y) (z,w) = x*z + y*w                 --return the dot product

todoMenor::(Float,Float)->(Float,Float)->Bool
todoMenor (x,y) (z,w) | x<z && y<w = True       --If both numbers from the first tuple are lower than the 2nd numbers, true
                      | otherwise = False

distanciaPuntos::(Float,Float)->(Float,Float)->Float
distanciaPuntos (x,y) (z,w) = sqrt (((x-z)*(x-z))+((y-w)*(y-w))) --Returns the distance between 2 pont in R2 hehe xd

sumaTerna::(Int,Int,Int)->Int
sumaTerna (x,y,z) = (x+y+z)

chequearMultiplo:: Int->Int->Int
chequearMultiplo x y | (mod (x) y)==0 =x --if the resto between the module of x and y is 0 (they are multiplos) return the number
                     | otherwise=0         --if not, return 0


sumarSoloMultiplos::(Int,Int,Int)->Int->Int
sumarSoloMultiplos (x,y,z) q=((chequearMultiplo x q)+(chequearMultiplo y q)+(chequearMultiplo z q)) --Sum the checks of the numbers

esPar::Int->Bool
esPar x| (mod x 2)==0=True    --If it's pair, returns true
        |otherwise =False       --Otherwise, return false

posPrimerPar:: (Int,Int,Int)->Int       --returns the position of the first pair number in the tern
posPrimerPar (x,y,z) |esPar x=1
                     |esPar y=2
                     |esPar z=3
                     |otherwise = 4

crearPar::String->String->(String,String)
crearPar x y=(x,y)

--Guia practica 4

fibonacci::Int->Int
fibonacci x | x==0=0
            | x==1=1
            |otherwise =(fibonacci (x-1) + fibonacci (x-2))

--Con pattern matching
--fibo:: Int->Int
--fibo  0=0
--fibo  1=1
--fibo  x=(fibonacci(x-1) + fibonacci(x-2))


parteEntera::Float->Int
parteEntera x | x>=0 && x>1=0
              | x>1 = parteEntera (x-1) +1
              | x <=0 = parteEntera (x+1) + 1
              | x<=0 = parteEntera (x+1) -1


esDivisible::Integer->Integer->Bool
esDivisible x y| x ==y =True
               |x>y = esDivisible x (y+y)
               |otherwise =False


--jercicio 4. Especificar e implementar la funci´on 
--sumaImpares :: Integer ->Integer que dado n ∈ N sume los primeros n n´umeros impares. Por ejemplo: sumaImpares 3 ❀ 1+3+5 ⇝ 9.

sumaImpares::Int->Int
sumaImpares x | x==1=1
              | otherwise =  sumaImpares (x-1) + ((2*x)-1)

--Ejercicio 5

medioFact::Integer->Integer
medioFact x | x==0 ||x==1 = 1
            |otherwise = medioFact (x-2) * x

--jercicio 6. Especificar e implementar la funci´on 
--sumaDigitos :: Integer ->Integer que calcula la suma de d´ıgitos de un n´umero natural. Para esta funci´on pueden utilizar div y mod.


sumaDigitos::Integer->Integer
sumaDigitos x | x<10 =x
              |otherwise = x

--10i

sumatoria2ICuadrado::Integer->Integer
sumatoria2ICuadrado x | x==0=1
                     | otherwise = sumatoria2ICuadrado (x-1) + (2^x)


sumatoriaQALAN::Integer->Integer->Integer
sumatoriaQALAN x y | x==1 = y^1
                   | otherwise= sumatoriaQALAN (x-1) y + (y^x)


--13    

sumatoriaDoble::Integer->Integer->Integer
sumatoriaDoble x y | x==0=0
                   | y==0=0
                   | otherwise =(sumatoriaQALAN y x) + sumatoriaDoble (x-1) y  -- + (x^y)


--16a Implementar menorDivisor :: Integer ->Integer que calcule el menor divisor (mayor que 1) de un natural n pasado
--como par´ametro.

divisorMinimo::Integer->Integer->Integer
divisorMinimo x y | mod x y == 0 = y
        |otherwise = divisorMinimo x (y+1)

menorDivisor::Integer->Integer
menorDivisor x = divisorMinimo x 2

--16b Implementar la funci´on esPrimo :: Integer ->Bool que indica si un n´umero natural pasado como par´ametro es primo

esPrimo::Integer->Bool
esPrimo x | menorDivisor x == x =True
          | otherwise =False

--16c  Implementar la funci´on sonCoprimos :: Integer ->Integer ->Bool que dados dos n´umeros naturales indica si no
--tienen alg´un divisor en com´un mayor estricto que 1.

comunDivisor::Integer->Integer->Integer->Bool
comunDivisor x y z|z>x && z>y =False
                  |mod x z ==0 && mod y z==0 = True
                  |otherwise= comunDivisor x y (z+1)

sonCoprimos::Integer->Integer->Bool
sonCoprimos x y = comunDivisor x y 2

--19 

siEsPrimo::Integer->Integer
siEsPrimo x |esPrimo x =x
            |otherwise = 0

sumaDePrimos::Integer->Integer->Integer
sumaDePrimos x i | i >= x = 0
                 | otherwise = sumaDePrimos x (i+1) + siEsPrimo x

esSumaInicialDePrimos::Integer->Bool
esSumaInicialDePrimos x | x== sumaDePrimos x 2 = True
                        | otherwise = False

--sumatoria4::Integer->Float->Float
--sumatoria4 x y | 

longitud::[Integer]->Integer
longitud []=0
longitud (_:xs)=1+ longitud xs

sumatoria::[Integer]->Integer
sumatoria []=0
sumatoria (x:xs)=x + sumatoria xs

pertenece::Eq t=> t ->[t]->Bool
pertenece _ []=False
pertenece x arr | x== head arr=True
                | otherwise = pertenece x (tail arr)



sacarBlancoRepetidos::[Char]->[Char] -- no anda
sacarBlancoRepetidos []=""
sacarBlancoRepetidos (x:y:xs) | x==' ' && y==x =sacarBlancoRepetidos (xs) ++[x]
                              |otherwise = sacarBlancoRepetidos (xs)++[y]++[x]

--2
ultimo::[t]->t
ultimo [x]=x
ultimo (x:xs) = ultimo xs

--3
principio::[Int]->[Int]
principio (x:xs) = ([x]++xs) ++ [0] ++ [(length ([x]++xs))-1]

--4
reverso::[t]->[t]
reverso []=[]
reverso (x:xs)=reverso xs ++ [x]

--2.1
pertenece2::Eq t => t->[t]->Bool
pertenece2 _ []=False
pertenece2 x (y:xs) | x == y =True
                    | otherwise = pertenece2 x xs

--2.2
todosIguales::Eq t=>[t]->Bool
todosIguales [x]=True
todosIguales (x:xs) | x== head xs = todosIguales xs
                    | otherwise = False

--2.3
recorredor::Eq t=>t->[t]->Bool
recorredor _ [x] =True
recorredor y (x:xs) |x /= y =recorredor y xs
                    |otherwise= False

todosDistintos::Eq t=>[t]->Bool
todosDistintos [x]=True
todosDistintos (x:xs)|recorredor x xs =todosDistintos xs
                     | otherwise = False --no anda

--2.5
quitarTodos::Eq t=>t->[t]->[t]
quitarTodos _ [] = []
quitarTodos x (y:xs) | x/=y =[y]++quitarTodos x xs
                     | otherwise= quitarTodos x xs


--2.6
quitar::Eq t=>t->[t]->[t]
quitar _ [] = []
quitar x (y:xs) | x==y = xs
                | otherwise=y: quitar x xs


--2.7
seRepite::Eq t=>t->[t]->Bool
seRepite _ [] = False
seRepite x (y:xs)|x==y=True
                 |otherwise=seRepite x xs

eliminarRepetidos:: Eq t=>[t]->[t]
eliminarRepetidos [] =[]
eliminarRepetidos (x:xs)| seRepite x xs  = eliminarRepetidos xs
                        | otherwise= eliminarRepetidos xs ++[x]


--2.8
mismosElementos::Eq t=> [t]->[t]->Bool
mismosElementos [] _ = True
mismosElementos _ [] =False
mismosElementos x y | pertenece (head x) (eliminarRepetidos y) && pertenece (head y) (eliminarRepetidos x) =mismosElementos (eliminarRepetidos (tail x)) (eliminarRepetidos (tail y))
                         | otherwise =False--ya no lo voy a intentar

--3.3
maximo::[Integer]->Integer
maximo [x]=x
maximo (x:xs)|esMayor x xs =x
             |otherwise = maximo xs

esMayor::Integer->[Integer]->Bool
esMayor x [y] | x > y = True
              |otherwise = False
esMayor x (y:ys)| x>y && esMayor x ys= True
                |otherwise= False


--3.9\
ordenar::[Integer]->[Integer]
ordenar []=[]
ordenar xs =  ordenar (quitar (maximo xs) xs) ++ [maximo xs]

--Parcial 2023
relacionesValidas::[(String,String)]->Bool
relacionesValidas [(x,y)] | x==y = False
                          | otherwise = True
relacionesValidas ((x,y):[(z,w)]) | (x==y) ||z==w || ((x==z && y==w)||(x==w && y==z)) = False
                                |otherwise = relacionesValidas [(x,y)]
relacionesValidas ((x,y):(z,w):xs) | (x==y) ||z==w || ((x==z && y==w)||(x==w && y==z)) = False
                                 | otherwise = relacionesValidas ((z,w):xs)

--

pertenece7::String->[(String,String)]->Bool
pertenece7 x [(y,z)]| x==y ||x==z =True
                    |otherwise = False
pertenece7 x ((y,z):xs)| x==y ||x==z = True
                       | otherwise = pertenece7 x xs


personas::[(String,String)]->[String]
personas [(x,y)]= [x]++[y]
personas ((x,y):xs)| pertenece7 x xs && pertenece7 y xs= personas xs
                   | pertenece7 x xs && not (pertenece7 y xs) =[y]++personas xs
                   | pertenece7 y xs && not (pertenece7 x xs) =[x]++ personas xs

--

amigosDe::String->[(String,String)]->[String]
amigosDe x [(y,z)]| x==y = [z]
                  | x==z = [y]
                  | otherwise = []
amigosDe x ((y,z):xs)|x==y = (amigosDe x xs)++[z]
                     |x==z = (amigosDe x xs)++[y]
                     |otherwise = amigosDe x xs


--

personas2::[(String,String)]->[String]
personas2 [(x,y)]= [x]++[y]
personas2 ((x,y):xs) = x:y:personas2 xs

repeticiones::String->[String]->Integer
repeticiones x [y]|x==y = 1
                  |otherwise = 0
repeticiones x (y:xs)| x==y = (repeticiones x xs) + 1
                     |otherwise = repeticiones x xs

elMas::[String]->String
elMas [x]= x
elMas [x,y] = x
elMas (x:y:xs)|repeticiones x xs == repeticiones y xs =  elMas (xs++[x]++[y])
              |repeticiones x xs > repeticiones y xs = elMas (xs++[x])
              |otherwise = elMas (y:xs)

personaConMasAmigos::[(String,String)]->String
personaConMasAmigos (x)=elMas (personas2 x)




--que es modelar? (abstraer un problema, se especifica y se disenia)
--que es una estructura de dato? (una forma de almacenar datos para modelar y formar nuevos tipos)
--diferencia entre tupla y lista (la tupla tiene tamanio definido, la lista no. son dos tipo de dato diferente)
--por que necesito la trivaluada? (porque nuestros problemas lidian con indefinidos)
--cuando una formula es mas fuerte que otra? (cuando mas valores e dan posibilidad de verdad)
--diferencias de falla, deficiencia y error (el error es el humano, la falla es que no anda como esperaba, la deficiencia es el componente deficiente)
--diferencias de test de caja blanca y caja negra (ambos sirven para hacer casos)
--que es hacer testing (ejecutar el programa y ver que hace)
--que es la transparencia referencial (una funcion depende solo y unicamente de sus parametros)
--que pasa si tengo un problema con 2 asegura que se contradicen
--es cierto que para que el programa cumpla el contrato hace falta que cumpla todos los requiere y todos los asegura
--para que sirven variables de tipo
--que son polimorfismos
--que son las clases de tipo (comparten operaciones)
--que es una ecuacion orientada (No es lo mismo invertir el orden)
--que es un programa funcional (Un conjunto de funciones)
--que es pattern matching (una forma de definir ecuaciones orientadas a traves de valores o construcctores a la izquierda del igual)
--que es reduccion ()
--cuando una funcion es recursiva (cuando una funcion se llama a si misma)
--que son tipos de dato (conjuntos de datos con distintas operaciones)
--sub especificada? (los asegura se salen del prblema original)
--sobre especificada? (Los requiere se salen del problema original)
--que es modular un problema? (disolver un problema grande en pequenios)
--cuando un programa cumple el contrato (el usuario sabe que va a andar bien mientras que cumpla con los requiere)

--Parcial democracia
--1

total::[Int]->Int
total [] = 0
total (x:xs)=x+ (total xs)

votosEnBlanco::[(String,String)]->[Int]->Int->Int
votosEnBlanco _ [] _ = 0
votosEnBlanco _ xs y = y - (total xs)

--2

aplanar::[(String,String)]->[String]
aplanar [] = []
aplanar ((x,y):xs)=x:y: aplanar xs

pertenece22::String->[String]->Bool
pertenece22 _ [] = False
pertenece22 x (y:xs)|x==y = True
                    | otherwise =  pertenece22 x xs

hayRepetidos::[String]->Bool
hayRepetidos [x]=False
hayRepetidos (x:xs)| pertenece22 x xs =True
                   | otherwise = hayRepetidos xs

formulasValidas::[(String,String)]->Bool
formulasValidas x=not (hayRepetidos (aplanar x))

--3

division::Int->Int->Float
division a b = (fromIntegral a) / (fromIntegral b)

porcentaje::Int->[Int]->Float
porcentaje x xs =( division x (total (xs++[x])) ) *100

porcentajeDeVotos::String->[(String,String)]->[Int]->Float
porcentajeDeVotos _ [] _ = 0
porcentajeDeVotos x ((a,b):xs) (y:ys)| x==a = porcentaje y ys
                                     |otherwise = porcentajeDeVotos x xs (ys++[y])

--4

proximoPresidente::[(String,String)]->[Int]->String
proximoPresidente [x] [uno]=fst x
proximoPresidente ((a,b):(c,d):xs) (y:z:ys)| y>z = proximoPresidente ((a,b):xs) (y:ys)
                                           | otherwise = proximoPresidente ((c,d):xs) (z:ys)


--parcial simulacro here we go again

--1
pertenece54::(String,String)->[(String,String)]->Bool
pertenece54 x [y]|(fst x==fst y) ||(fst x == snd y)||(fst y==snd y)=True
                 |otherwise =False
pertenece54 x (y:ys)|(fst x==fst y) ||(fst x == snd y)||(fst y==snd y)= True
                    |otherwise = pertenece54 x ys

relacionesValidas2::[(String,String)]->Bool
relacionesValidas2 [x]|fst x == snd x =False
                      | otherwise = True
relacionesValidas2 (x:xs)| pertenece54 x xs = False
                         | otherwise = relacionesValidas2 xs

--2

aplanar2::[(String,String)]->[String]
aplanar2 [x] = [fst x]++[snd x]
aplanar2 (x:xs)=[fst x]++[snd x]++ aplanar2 xs

pertenece90:: String->[String]->Bool
pertenece90 _ [] = False
pertenece90 x (y:ys) | x==y = True
                     |otherwise = pertenece90 x ys

sacarRepetidos::[String]->[String]
sacarRepetidos []=[]
sacarRepetidos (x:xs)| pertenece90 x xs = sacarRepetidos xs
                     |otherwise = [x] ++ sacarRepetidos xs

personas3::[(String,String)]->[String]
personas3 x = sacarRepetidos (aplanar2 x)

--3

amigosDe2::String->[(String,String)]->[String]
amigosDe2 _ [] = []
amigosDe2 x (y:ys)| x== fst y=[snd y]++ amigosDe2 x ys
                  | x== snd y = [fst y] ++ amigosDe2 x ys
                  |otherwise = amigosDe2 x ys


--4

longitudString::[String]->Int
longitudString []=0
longitudString (x:xs)=1+longitudString xs

aplanadaSinRepetir::[(String,String)]->[String]
aplanadaSinRepetir x = sacarRepetidos (aplanar2 x)

comparar::[String]->[(String,String)]->String
comparar [x] _ = x
comparar [x,y] ys |longitudString (amigosDe2 x ys) >= longitudString (amigosDe2 y ys) = x
                  |otherwise = y
comparar (x:y:xs) ys | longitudString (amigosDe2 x ys) >= longitudString (amigosDe2 y ys) = comparar (x:xs) ys
                     |otherwise = comparar (y:xs) ys


personaConMasAmigos2::[(String,String)]->String
personaConMasAmigos2 x = comparar (aplanadaSinRepetir x) x

--Practica 4
--ej 2.4

pertenece3:: Eq t=> t->[t]->Bool
pertenece3 _ []=False
pertenece3 x (y:ys)|x==y = True
                   |otherwise = pertenece3 x ys

hayRepetidos2::Eq t=>[t]->Bool
hayRepetidos2 []= False
hayRepetidos2 (x:xs) | pertenece3 x xs =True
                     | otherwise = hayRepetidos2 xs

--2.9
capicua::Eq t=>[t]->Bool
capicua x| x== reverso x=True
         | otherwise = False


--ejercicio 3
--3.1

sumatoria2::[Integer]->Integer
sumatoria2 []=0
sumatoria2 (x:xs) = x+sumatoria2 xs

--3.2

productoria::[Integer]->Integer
productoria [] = 1
productoria (x:xs) =x*productoria xs

--3.3
maximo2::[Integer]->Integer
maximo2 [x]=x
maximo2 (x:y:xs)| x >= y = maximo2 (x:xs)
                |otherwise = maximo2 (y:xs)


--3.4

sumarN::Integer->[Integer]->[Integer]
sumarN _ []=[]
sumarN x (y:ys)=[x+y]++sumarN x ys

--3.5

sumarElPrimero::[Integer]->[Integer]
sumarElPrimero (x:xs)=sumarN x (x:xs)


--3.6

sumarlos::[Integer]->[Integer]->[Integer]
sumarlos [x] xs = sumarN x xs
sumarlos (x:xs) ys = sumarlos xs ys

sumarElUltimo::[Integer]->[Integer]
sumarElUltimo x = sumarlos x x

--3.7

--Parcial maniana 2024 1er cuatri

--1
hayQueCodificar::Char->[(Char,Char)]->Bool
hayQueCodificar _ [] = False
hayQueCodificar x (y:ys) | x == (fst y) =True
                         | otherwise = hayQueCodificar x ys

--2

contarCuantas::Char->[Char]->Int
contarCuantas x [] = 0
contarCuantas x (y:ys) | x==y = 1+ contarCuantas x ys
                       | otherwise = contarCuantas x ys

cuantasVecesHayQueCodificar::Char->[Char]->[(Char,Char)]->Int
cuantasVecesHayQueCodificar x ys zs | hayQueCodificar x zs = contarCuantas x ys
                                    | otherwise = 0


--3
laMasRepetida::[Char]->[Char]->Char
laMasRepetida _ [x] =x
laMasRepetida xs (x:y:ys)| contarCuantas x xs > contarCuantas y xs= laMasRepetida xs (x:ys)
                         | otherwise = laMasRepetida xs (y:ys)

sacarRepetidos90::[Char]->[Char]
sacarRepetidos90 []=[]
sacarRepetidos90 (x:xs)| pertenece73 x xs = sacarRepetidos90 xs
                       | otherwise =  (sacarRepetidos90 xs)++[x]

pertenece73::Char->[Char]->Bool
pertenece73 _ [] = False
pertenece73 x (y:ys)| x==y = True
                    |otherwise = pertenece73 x ys


laQueMasHayQueCodificar::[Char]->[(Char,Char)]->Char
laQueMasHayQueCodificar x _ = laMasRepetida x (sacarRepetidos90 x)

--4

encontrarPareja::Char->[(Char,Char)]->Char
encontrarPareja x ((a,b):ys)| x==a = b
                            | otherwise = encontrarPareja x ys

codificarFrase::[Char]->[(Char,Char)]->[Char]
codificarFrase [] _  = []
codificarFrase (x:xs) cs | hayQueCodificar x cs = [encontrarPareja x cs]++ codificarFrase xs cs
                         | otherwise = [x] ++ codificarFrase xs cs



--Parcial de la tarde 1er cuatri 2024

--1

cantidadMaterias::[Int]->Int
cantidadMaterias [] = 0
cantidadMaterias (x:xs)  | x >= 4 = 1 + cantidadMaterias xs
                         | otherwise = cantidadMaterias xs

aproboMasDeNMaterias::[([Char],[Int])]->[Char]->Int->Bool
aproboMasDeNMaterias [] _ _ = False
aproboMasDeNMaterias ((alumno, notas):xs) miAlumno cantidadNotas | alumno == miAlumno && (cantidadMaterias notas) > cantidadNotas =True
                                                                 | otherwise = aproboMasDeNMaterias xs miAlumno cantidadNotas

--2

longitud123::[Int]->Int
longitud123 [] = 0
longitud123 (x:xs)= 1 + longitud123 xs

sumaDe::[Int]->Int
sumaDe [] = 0
sumaDe (x:xs) = x + (sumaDe xs)

promedio::[Int]->Float
promedio x = (fromIntegral (sumaDe x)) / (fromIntegral (longitud123 x))

tieneAplazos::[Int]->Bool
tieneAplazos [x]|x<4= True
                |otherwise = False
tieneAplazos (x:xs)| x<4= True
                   | otherwise = tieneAplazos xs

buenosAlumnos::[([Char],[Int])]->[[Char]]
buenosAlumnos []  = []
buenosAlumnos ((a,b):xs)| (promedio b >= 8)&& not (tieneAplazos b) = [a]++(buenosAlumnos xs)
                        | otherwise = buenosAlumnos xs


--3

mejorPromedio::[([Char],[Int])]->[Char]
mejorPromedio [x] = fst x
mejorPromedio ((a,b):(c,d):xs)| promedio b >= promedio d = mejorPromedio ((a,b):xs)
                              | otherwise = mejorPromedio ((c,d):xs)

--4

mejorPromedio2::[([Char],[Int])]->[Int]
mejorPromedio2 [x] = snd x
mejorPromedio2 ((a,b):(c,d):xs)| promedio b >= promedio d = mejorPromedio2 ((a,b):xs)
                              | otherwise = mejorPromedio2 ((c,d):xs)

promedioSirve::[Int]->[([Char],[Int])]->Bool
promedioSirve x ys| (promedio (mejorPromedio2 ys)) - promedio x < 1 = True
                  | otherwise = False

seGraduoConHonores::[([Char],[Int])]->Int->[Char]->Bool
seGraduoConHonores ((a,b):xs) cantMaterias alumno | (a==alumno) && (promedio b >=8) && not (tieneAplazos b) && (aproboMasDeNMaterias [(a,b)] alumno (cantMaterias - 1)) && (promedioSirve b ((a,b):xs)) = True
                                                  | (a==alumno) && (not ((promedio b >=8))|| tieneAplazos b || not (aproboMasDeNMaterias [(a,b)] alumno (cantMaterias - 1)) || not (promedioSirve b ((a,b):xs))) = False
                                                  | otherwise = seGraduoConHonores (xs++[(a,b)]) cantMaterias alumno


--2do Parcial de la maniana 

--1

repeticiones34::[Char]->[[Char]]->Int
repeticiones34 _ []=0
repeticiones34 x (y:ys) | x==y = 1+ repeticiones34 x ys
                      | otherwise = repeticiones34 x ys

calcularRepeticion::[Char]->[[Char]]->[([Char],Int)]
calcularRepeticion x  xs=[(x,repeticiones34 x xs)]

pertenece4::[Char]->[[Char]]->Bool
pertenece4 _ [] = False
pertenece4 x (y:ys) | x==y = True
                    | otherwise = pertenece4 x ys

sacarRepetidos2::[[Char]]->[[Char]]
sacarRepetidos2 []=[]
sacarRepetidos2 (x:xs)| pertenece4 x xs = sacarRepetidos2 xs
                      | otherwise = [x]++ (sacarRepetidos2 xs)

generarStock::[[Char]]->[([Char],Int)]
generarStock x = generarAux (sacarRepetidos2 x) x

generarAux::[[Char]]->[[Char]]->[([Char],Int)]
generarAux [] _ = []
generarAux (x:xs) ys =( calcularRepeticion x ys )++generarAux xs ys

--2

stockDeProducto::[([Char],Int)]->[Char]->Int
stockDeProducto [] _ = 0
stockDeProducto (x:xs) y | fst x == y = snd x
                         | otherwise = stockDeProducto xs y


--3

dineroEnStock::[([Char],Int)]->[([Char],Float)]->Float
dineroEnStock [] _ = 0
dineroEnStock (x:xs) (y:ys) | fst x == fst y = ((fromIntegral (snd x)) * (snd y)) + dineroEnStock xs (ys++[y])
                            | otherwise = dineroEnStock (x:xs) (ys++[y])

--4

aplicarOferta::[([Char],Int)]->[([Char],Float)]->[([Char],Float)]
aplicarOferta [] _ = []
aplicarOferta (x:xs) (y:ys) | (fst x == fst y) && (snd x > 10) = [(fst x, ((fromIntegral (snd x)) * 0.8 * snd y ))] ++ (aplicarOferta xs (ys++[y]))
                            | (fst x == fst y) && (snd x <= 10) = [(fst x, ((fromIntegral (snd x)) * snd y ))] ++ (aplicarOferta xs (ys++[y]))
                            | otherwise = aplicarOferta (x:xs) (ys++[y])




--tp
--Pautas
--2) hay que implementar un conjuntos de tests para todos los ejercicios con la mayor cantidad de casos posibles. Tienen que tener sentido (se tienen que entregar)
--3) el archivo con el codigo fuente se tiene que llamar Solucion.hs
--4) El codigo debe ser ejecutable en las pc's del labo
--5) No se puede cambiar el nombre de las funciones ni los tipos de datos de entrada
--6) Se pueden usar todas las funciones auxiliares que quieras
--7) No dos veces la misma funcion (reutilizar codigo onegaishimasu)
--8) (Recomendable) Testear cruzado
--9) No se pueden usar cosas que no vimos en la teorica o en la practica
--
-- Comentar los casos de los tests
-- Se entrega por gitlab
-- ejercicio 10, donde dice decifrado es decifrar 

-- Parcial 1

--1

pertenece5:: [Char]-> [[Char]]->Bool
pertenece5 _ [] = False
pertenece5 z (x:xs) | x==z =True
                    | otherwise = pertenece5 z xs

sacarRepetidos3:: [[Char]]->[[Char]]
sacarRepetidos3 []=[]
sacarRepetidos3 (x:xs) | pertenece5 x xs = sacarRepetidos3 xs
                       | otherwise = [x] ++ sacarRepetidos3 xs

repeticiones2::[Char]->[[Char]]->Int
repeticiones2 _ [] = 0
repeticiones2 producto (x:xs) | producto == x = 1 + repeticiones2 producto xs
                              | otherwise = repeticiones2 producto xs

generarAux2::[[Char]]->[[Char]]->[([Char],Int)]
generarAux2 _ [] = []
generarAux2  productosOg (producto:productosSr) = [(producto, repeticiones2 producto productosOg)] ++ generarAux2 productosOg productosSr

generarStock2::[[Char]]->[([Char], Int)]
generarStock2 productos = generarAux2 productos (sacarRepetidos3 productos)


--2

stockDeProducto2::[([Char],Int)]->[Char]->Int
stockDeProducto2 [] _ = 0
stockDeProducto2 ((item,cantidad):resto) producto | producto == item = cantidad
                                                  | otherwise = (stockDeProducto2 resto producto)

--3

buscarPrecio::([Char],Int)->[([Char],Float)]->Float
buscarPrecio (producto,cantidad) ((nombre,precio):resto) | producto==nombre = (fromIntegral cantidad * precio)
                                                         | otherwise = (buscarPrecio (producto,cantidad) resto)

dineroEnStock2::[([Char],Int)]->[([Char],Float)]->Float
dineroEnStock2 [] _ = 0
dineroEnStock2 (producto:stock) precios= (buscarPrecio producto precios ) + (dineroEnStock2 stock precios)

--4

aplicarOferta2::[([Char],Int)]->[([Char],Float)]->[([Char],Float)]
aplicarOferta2 _ [] = []
aplicarOferta2 ((producto,cantidad):stock) ((nombre,precio):precios) | (producto==nombre) && (cantidad>10) = [(nombre,(precio * 0.8))] ++ (aplicarOferta2 stock precios)
                                                                     | (producto==nombre) && (cantidad<=10) = [(nombre, (precio))] ++ (aplicarOferta2 stock precios)
                                                                     | otherwise = aplicarOferta2 ((producto,cantidad):stock) (precios ++ [(nombre,precio)])


        

--VAMOS CAMPEÓN!
--En Exactas se está jugando un torneo de futbol y la facultad le pidió a los alumnos de IP programar algunas funcionalidades en Haskell.
--Los datos con los que contamos para esto son los nombres de los equipos que participan del torneo, los nombres de los goleadores de cada
--uno de dichos equipo, y la cantidad de goles convertidos por esos jugadores. Los nombres de los equipos y sus respectivos goleadores serán
--modelados mediante tuplas de tipo (String,String), donde la primera componente representa el nombre del equipo, y la segunda representa el
--nombre del goleador de dicho equipo.

--En los problemas en los cuales se reciban, como parámetros, secuencias _goleadoresPorEquipo_ y _goles_, cada posicion de la lista goles representará
--la cantidad de goles obtenidos por el goleador del equipo que se encuentra en esa misma posición de _goleadoresPorEquipo_.
--Por ejemplo si la lista goleadoresPorEquipo es [("Sacachispas","Robertino Giacomini"),("Fénix","Matias Dominguez")] y la lista goles es [3,5], eso indica
--que Robertino Giacomini metió 3 goles y Matias Dominguez metió 5.

----------------------------------------------------------------------------------------------------------------------------------------------------------

--1) Goles de no goleadores [1 punto]

--problema golesDeNoGoleadores (goleadoresPorEquipo: seq⟨String x String⟩, goles: seq⟨Z⟩, totalGolesTorneo: Z ): Z {
--    requiere: {equiposValidos(goleadoresPorEquipo)}
--    requiere: {|goleadoresPorEquipo| = |goles|}
--    requiere: {Todos los elementos de goles son mayores o iguales a 0}
--    requiere: {La suma de todos los elementos de goles es menor o igual a totalGolesTorneo}
--    asegura: {res es la cantidad de goles convertidos en el torneo por jugadores que no son los goleadores de sus equipos}
--}

--1
sumatoria3::[Int]->Int
sumatoria3 [] = 0
sumatoria3 (num:resto) =num + sumatoria3 resto

golesDeNoGoleadores::[(String,String)]->[Int]->Int->Int
golesDeNoGoleadores banana goles total= total - (sumatoria3 goles)

--2

--2) Equipos Válidos [3 puntos]

--problema equiposValidos (goleadoresPorEquipo: seq⟨String x String⟩): Bool{
--    requiere: {True}
--    asegura: {(res = True) <-> goleadoresPorEquipo no contiene nombres de clubes repetidos, ni goleadores repetidos, ni jugadores con nombre de club}
--}

pertenece6::String->[String]->Bool
pertenece6 _ [] = False
pertenece6 elem (x:xs)| elem == x = True
                      | otherwise = pertenece6 elem xs

hayRepetidos3::[String]->Bool
hayRepetidos3 [] = False
hayRepetidos3 (x:xs) | pertenece6 x xs = True
                     | otherwise = hayRepetidos3 xs

aplanar3::[(String,String)]->[String]
aplanar3 []=[]
aplanar3 ((x,y):xs) = x:y:(aplanar xs)

equiposValidos::[(String,String)]->Bool
equiposValidos lista = not(hayRepetidos3 (aplanar3 lista))

--3

--3) Porcentaje de Goles [3 puntos]

--problema porcentajeDeGoles (goleador: String, goleadoresPorEquipo: seq⟨String x String⟩, goles: seq⟨Z⟩): R {
--    requiere: {La segunda componente de algún elemento de goleadoresPorEquipo es goleador}
--    requiere: {equiposValidos(goleadoresPorEquipo)}
--    requiere: {|goleadoresPorEquipo| = |goles|}
--    requiere: {Todos los elementos de goles son mayores o iguales a 0}
--    requiere: {Hay al menos un elemento de goles mayor estricto a 0}
--    asegura: {res es el porcentaje de goles que marcó goleador sobre el total de goles convertidos por goleadores}
--}

--Para resolver este ejercicio pueden utilizar la siguiente función que devuelve como Float la división entre dos numeros de tipo Int:
division2 :: Int -> Int -> Float
division2 a b = (fromIntegral a) / (fromIntegral b)

sumatoria4::[Int]->Int
sumatoria4 [] = 0
sumatoria4 (x:xs) = x + sumatoria4 xs

calcularProrcentaje::Int->[Int]->Float
calcularProrcentaje gol goles = (division2 gol (sumatoria4 goles))*100

encontrarGol::String->[(String,String)]->[Int]->Int
encontrarGol goleador ((club,pj):resto) (gol:restoGoles)| goleador == pj = gol
                                                        | otherwise = encontrarGol goleador resto restoGoles

porcentajeDeGoles::String->[(String,String)]->[Int]->Float
porcentajeDeGoles goleador goleadoresPorEquipo goles = calcularProrcentaje (encontrarGol goleador goleadoresPorEquipo goles) goles

--4

--4) Botín de Oro [3 puntos]

--problema botinDeOro (goleadoresPorEquipo: seq⟨String x String⟩, goles: seq⟨Z⟩): String {
--    requiere: {equiposValidos(goleadoresPorEquipo)}
--    requiere: {|goleadoresPorEquipo| = |goles|}
--    requiere: {Todos los elementos de goles son mayores o iguales a 0}
--    requiere: {|goles| > 0}
--    asegura: {res es alguno de los goleadores de goleadoresPorEquipo que más tantos convirtió de acuerdo a goles}
--}

botinDeOro::[(String,String)]->[Int]->String
botinDeOro [(x,y)] [gol] = y
botinDeOro (x:xs) (gol:goles)| gol >= head(goles) = botinDeOro (xs++[x]) (goles++[gol])
                             | otherwise = botinDeOro xs goles

{-
1) Atajaron Suplentes
problema atajaronSuplentes (arquerosPorEquipo: seq<String X String>, goles: seq<Z>, totalGolesTorneo: Z): Z {
	requiere: {equiposValidos(arquerosPorEquipo)
	requiere: {|arquerosPorEquipo| = |goles|}
	requiere: {Todos los elementos de goles son mayores o iguales a 0}
	requiere: {La suma de todos los elementos de goles es menor o igual a totalGolesTorneo}
	asegura: {
	res es la cantidad de goles recibidos en el torneo por arqueros que no son titulares en sus equipos.
	}
}-}

sumatoria5::[Int]->Int
sumatoria5 [] = 0
sumatoria5 (gol:goles) = gol + sumatoria5 goles

atajaronSuplentes::[(String,String)]->[Int]->Int->Int
atajaronSuplentes _ golesArr goles = goles -(sumatoria5 golesArr)


{-2) Equipos Válidos
problema equiposValidos (arquerosPorEquipo: seq<String X String>): Bool {
	requiere: {True}
	asegura: {
	(res = True) <=> arquerosPorEquipo no contiene nombres de clubes repetidos, ni arqueros repetidos, ni jugadores con nombre del club
	}
-}

pertenece8::String->[String]->Bool
pertenece8 _ []  = False
pertenece8 x (c:cz) | x==c = True
                    | otherwise = pertenece8 x cz


hayRepetidos4::[String]->Bool
hayRepetidos4 [] = False
hayRepetidos4 (x:xs)| pertenece8 x xs = True
                    | otherwise = hayRepetidos4 xs

aplanar4::[(String,String)]->[String]
aplanar4 [] = []
aplanar4 (x:xs) = [fst x] ++ [snd x] ++ aplanar4 xs

equiposValidos2::[(String,String)]->Bool
equiposValidos2 equipos = not(hayRepetidos4 (aplanar4 equipos))

{-3) Porcentaje de goles
problema porcentajeDeGoles (arquero: String, arquerosPorEquipo: seq<String X String>, goles: seq<Z>): R {
	requiere: {La segunda componente de algún elemento de arquerosPorEquipo es arquero}
	requiere: {equiposValidos(arquerosPorEquipo)}
	requiere: {|arquerosPorEquipo| = |goles|}
	requiere: {Todos los elementos de goles son mayores o iguales a 0}
	requiere: {Hay al menos un elemento de goles mayores estricto a 0}
	asegura: {
	res es el porcentaje de goles que recibió arquero sobre el total de goles recibidos por arqueros titulares
	}
}

Para resolver este ejercicio pueden utilizar la siguiente función que devuelve como float la división entre dos
numeros de tipo Int.

division :: Int -> Int -> Float
division a b = fromIntegral a / fromIntegral b
-}

sumatoria6::[Int]->Int
sumatoria6 [] =0
sumatoria6 (x:xs) =x + sumatoria6 xs

calcularProrcentaje2::Int->[Int]->Float
calcularProrcentaje2 gol goles = (division gol (sumatoria6 goles))*100

porcentajeDeGoles2::String->[(String,String)]->[Int]->Float
porcentajeDeGoles2 arquero (x:xs) (y:ys)| arquero == snd x = calcularProrcentaje2 y (y:ys)
                                        | otherwise = porcentajeDeGoles2 arquero (xs++[x]) (ys++[y]) 

{-4) Valla Menos Vencida
problema vallaMenosVencida (arquerosPorEquipo: seq<String X String>, goles: seq<Z>): String {
	requiere: {equiposValidos(arquerosPorEquipo)}
	requiere: {|arquerosPorEquipo| = |goles|}
	requiere: {Todos los elementos de goles son mayores o iguales a 0}
	requiere: {|goles| > 0}
	asegura: {
	res es alguno de los arqueros de arquerosPorEquipo que menor goles recibió de acuerdo a goles
-}

vallaMenosVencida::[(String,String)]->[Int]->String
vallaMenosVencida [arquero] _ = snd arquero
vallaMenosVencida (arquero:arqueros) (gol:goles)| gol<=(head goles) =vallaMenosVencida (arqueros++[arquero]) (goles++[gol]) 
                                                | otherwise = vallaMenosVencida arqueros goles

