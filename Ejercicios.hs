{-

Notas previas

En algunos ejercicios se van a utilizar algunas de las funciones que están en el Prelude por ej: 

mod 20 3 = 2	el resto de la división entre 20 y 3 es 2. 
div 14 3 = 4	parte entera de la división entre 14 y 3 es 4. 
max 8 10 = 10	devuelve el max entre 2 números. 
min 9 15 = 9	devuelve el min entre 2 números. 

-}

--Punto 1

{-
Definir la función esMultiploDeTres/1, que devuelve True si un número es múltiplo de 3, p.ej: 
Main> esMultiploDeTres 9 
True 
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
import Distribution.Simple.Setup (TestShowDetails)
import System.Win32 (xBUTTON1)
{-# HLINT ignore "Eta reduce" #-}

esMultiploDeTres :: Integer -> Bool
esMultiploDeTres x = mod x 3 == 0

--Punto 2

{-
Definir la función esMultiploDe/2, que devuelve True si el segundo es múltiplo del primero, p.ej. 
Main> esMultiploDe 3 12
True
-}

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y = mod y x == 0

--Punto 3
{-
Definir la función cubo/1, devuelve el cubo de un número.
-}

cubo :: Integer -> Integer
cubo x = x^3

--Punto 4

{-
Definir la función area/2, devuelve el área de un rectángulo a partir de su base y su altura.
-}

area :: Double -> Double -> Double
area base altura = (altura*base)/2

--Punto 5

{-
Definir la función esBisiesto/1, indica si un año es bisiesto. 
(Un año es bisiesto si es divisible por 400 o es divisible por 4 pero no es divisible por 100) 
Nota: Resolverlo reutilizando la función esMultiploDe/2
-}

esBisiesto :: Integer -> Bool
esBisiesto x = esMultiploDe 400 x || esMultiploDe 4 x

--Punto 6

{-
Definir la función celsiusToFahr/1, pasa una temperatura en grados Celsius a grados Fahrenheit.
-}

celsiusToFahr :: Double -> Double
celsiusToFahr x = (x*1.8) + 32

--Punto 7

{-
Definir la función fahrToCelsius/1, la inversa de la anterior.
-}

fahrToCelsius :: Double -> Double
fahrToCelsius x = (x - 32) * 5/9

--Punto 8

{-
Definir la función haceFrioF/1, indica si una temperatura expresada en grados Fahrenheit es fría. 
Decimos que hace frío si la temperatura es menor a 8 grados Celsius. 
-}

haceFrio :: Double -> Bool
haceFrio x = fahrToCelsius x <= 8

--Punto 9

{-
Definir la función mcm/2 que devuelva el mínimo común múltiplo entre dos números, de acuerdo a esta fórmula. 
m.c.m.(a, b) = {a * b} / {m.c.d.(a, b)} 
-}

mcm :: Int -> Int -> Int
mcm x y = (x * y) `div` mcd x y

mcd :: Int -> Int -> Int
mcd a 0 = a
mcd a b = mcd b (a `mod` b)


{-
mcm :: Double -> Double -> Double
mcm x y = ( x * y ) / mcd x y

mcd :: Double -> Double -> Double
mcd a 0 = a
mcd a b = mcd b (a `mod` b)
-}

--Punto 10

{-
Dispersión
Trabajamos con tres números que imaginamos como el nivel del río Paraná a la altura de 
Corrientes medido en tres días consecutivos; cada medición es un entero que representa una cantidad de cm. 

P.ej. medí los días 1, 2 y 3, las mediciones son: 322 cm, 283 cm, y 294 cm. 
A partir de estos tres números, podemos obtener algunas conclusiones. 

Definir estas funciones: 

a) dispersion, que toma los tres valores y devuelve la diferencia entre el más alto y el más bajo. 
Ayuda: extender max y min a tres argumentos, usando las versiones de dos elementos.
De esa forma se puede definir dispersión sin escribir ninguna guarda (las guardas están en max y min, que estamos usando). 

b) diasParejos, diasLocos y diasNormales reciben los valores de los tres días. 
Se dice que son días parejos si la dispersión es chica, que son días locos si la dispersión es grande, y 
que son días normales si no son ni parejos ni locos. 
Una dispersión se considera chica si es de menos de 30 cm, y grande si es de más de un metro. 
Nota: Definir diasNormales a partir de las otras dos, no volver a hacer las cuentas. 

-}

data Parametros = Parametros {nivel :: (Int,Int,Int)}

dispersion :: Parametros -> (Int , String)
dispersion (Parametros (x , y , z))  =  (diferenciaMaxMin (Parametros (x ,y, z)) , diasNormales (Parametros (x ,y , z)))

diferenciaMaxMin :: Parametros -> Int
diferenciaMaxMin (Parametros (x ,y ,z))  = max (max x y) z - min ( min x y) z

diasNormales :: Parametros -> String
diasNormales (Parametros(x,y,z))
    |diferenciaMaxMin (Parametros (x,y,z)) > 30 = "a"
    |diferenciaMaxMin (Parametros (x,y,z)) < 30 = "a"
    |diferenciaMaxMin (Parametros (x,y,z)) > 100 = "a"

--Punto 11

{-
En una plantación de pinos, de cada árbol se conoce la altura expresada en cm. El peso de un pino se puede calcular 
a partir de la altura así: 3 kg x cm hasta 3 metros, 2 kg x cm arriba de los 3 metros. P.ej. 2 metros ⇒  600 kg, 5 metros ⇒  1300 kg. 
Los pinos se usan para llevarlos a una fábrica de muebles, a la que le sirven árboles de entre 400 y 1000 kilos, un pino 
fuera de este rango no le sirve a la fábrica. Para esta situación: 


a) Definir la función pesoPino, recibe la altura de un pino y devuelve su peso. 

b) Definir la función esPesoUtil, recibe un peso en kg y devuelve True si un pino de ese peso le sirve a la fábrica, y False en caso contrario. 

c) Definir la función sirvePino, recibe la altura de un pino y devuelve True si un pino de ese peso le sirve a la fábrica, y False en caso contrario. Usar composición en la definición. 

-}

pesoPino :: Integer -> Integer
pesoPino x | x <= 300 = div (x*3)  1
           | otherwise = div (x*2) 1

esPesoUtil :: Integer -> Bool
esPesoUtil x = x > 400 && x < 1000

sirvePino :: Integer -> Bool
sirvePino x = pesoPino x > 400 && pesoPino x < 1000

{-
Aplicación Parcial
Nota previa Hay aplicación parcial cuando a una función la evaluamos con menor cantidad de argumentos con la que está definida. 
Veamos un Ejemplo: 
La función (*) recibe dos enteros como argumento y devuelve el resultado de la multiplicación. 
(*)::Int->Int->Int 
Si queremos aplicar la función (*) con un solo argumento nos quedaría: 
(2*) Esto nos devuelve una función que es (Int->Int), recibe un entero y devuelve otro entero. 
Si esta última función la aplico sobre un entero, nos devuelve otro entero como resultado Ej: Main> (2*) 9 18 
-}

--Punto 1 

{-
Definir una función siguiente, que al invocarla con un número cualquiera me devuelve el resultado de sumar a ese número el 1. 
Main> siguiente 3
4
-}

siguiente :: Integer -> Integer
siguiente x = x + 1

avanza = siguiente

--Puedo hacer "igualdad" de una funcion a otra ya existente.

--Punto 2

{-
Definir la función mitad que al invocarla con un número cualquiera me devuelve la mitad de dicho número, ej: 
Main> mitad 5
2.5
-}

mitad :: Double -> Double
mitad x = x / 2

--Punto 3

{-
Definir una función inversa, que invocando a la función con un número cualquiera me devuelva su inversa. 
Main> inversa 4
0.25
Main> inversa 0.5
2.0
-}

inversa :: Double -> Double
inversa x = 1 / x

--Punto 4

{-
Definir una función triple, que invocando a la función con un número cualquiera me devuelva el triple del mismo.
Main> triple 5 
15
-}

triple :: Integer -> Integer
triple x = 3 * x

--Punto 5

{-
Definir una función esNumeroPositivo, que invocando a la función con un número cualquiera me devuelva true si el número es positivo y false en caso contrario. 
Main> esNumeroPositivo (-5)
False
Main> esNumeroPositivo 0.99
True 
-}

esNumeroPositivo :: Double -> Bool
esNumeroPositivo x = x > 0

{-
Composición 
Nota previa 
Se base en el concepto matemático de composición de funciones. 
g(f(x)) = (g o f) x 
La imagen de f(x) tiene que coincidir con el dominio de g(x) 

Acá la notación es (g . f) x . Veamos un Ejemplo: 
g n = even n 
f n = 3 + n 

Main> (g . f) 4 
False 
Main> (g . f) 3
True
-}

--Punto 6

{-
Resolver la función del ejercicio 2 de la guía anterior esMultiploDe/2, utilizando aplicación parcial y composición.

Definir la función esMultiploDe/2, que devuelve True si el segundo es múltiplo del primero, p.ej. 
Main> esMultiploDe 3 12
True
esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y = mod y x == 0

-}

esMultiploDe1 :: Integer -> Bool
esMultiploDe1 x = mod x (siguiente x) == 0

--Punto 7

{-
Resolver la función del ejercicio 5 de la guía anterior esBisiesto/1, utilizando aplicación parcial y composición.

Definir la función esBisiesto/1, indica si un año es bisiesto. 
(Un año es bisiesto si es divisible por 400 o es divisible por 4 pero no es divisible por 100) 
Nota: Resolverlo reutilizando la función esMultiploDe/2


esBisiesto :: Integer -> Bool
esBisiesto x = esMultiploDe 400 x || esMultiploDe 4 x
-}

--Punto 8
 {-
 Resolver la función inversaRaizCuadrada/1, que da un número n devolver la inversa su raíz cuadrada. 
 Main> inversaRaizCuadrada 4 
 0.5 
 Nota: Resolverlo utilizando la función inversa Ej. 2.3, sqrt y composición.
 -}

inversaRaizCuadrada :: Double -> Double
inversaRaizCuadrada x = inversa (sqrt x)

--Punto 9

{-
Definir una función incrementMCuadradoN, que invocándola con 2 números m y n, incrementa un valor m al cuadrado de n por Ej: 
Main> incrementMCuadradoN 3 2 
11 
Incrementa 2 al cuadrado de 3, da como resultado 11. Nota: Resolverlo utilizando aplicación parcial y composición. 
-}

cuadrado :: Integer -> Integer
cuadrado x = x ^ 2

incrementMCuadradoN :: Integer -> Integer -> Integer
incrementMCuadradoN x y = cuadrado x + y

--Punto 10

{-
Definir una función esResultadoPar/2, que invocándola con número n y otro m, devuelve true si el resultado de elevar n a m es par. 
Main> esResultadoPar 2 5 
True 
Main> esResultadoPar 3 2
False 
Nota Obvia: Resolverlo utilizando aplicación parcial y composición.
-}

esResultadoPar :: Integer -> Integer -> Bool
esResultadoPar x y = even (x^y)

{-
Tuplas
Una tupla es un tipo de dato compuesto, que contiene una cantidad fija de elementos y se pueden combinar distintos tipos de elementos, cada 
elemento está en una posición fija de la estructura. 
Ej: Un alumno viene dado por una tupla de 2 elementos, que está formada por el (legajo, promedio). 
alumno = (“1104433-8”, 9). 
Existen algunas funciones predefinidas para obtener cada uno de los elementos de la tupla. Por Ej. si quiero obtener el legajo del alumno puedo hacer lo siguiente: 
Main> fst alumno 
"1104433-8" 
Y si quiero obtener el promedio: 
Main> snd alumno 
9 
Otros Ejemplo: 
fst(4,6) = 4 -- devuelve el primer elemento de la tupla.
snd(5,7) = 7 -- devuelve el segundo elemento de la tupla.
-}

-- Punto 1

{-
Definir las funciones fst3, snd3, trd3, que dada una tupla de 3 elementos devuelva el elemento correspondiente, 
p.ej. 
Main> snd3 (4,5,6) 
5
Main> trd3(4,5,6)
6
-}

fst3 :: (Integer,Integer,Integer) -> Integer
fst3 ( x , y , z ) = x

snd3 :: (Integer,Integer,Integer) -> Integer
snd3 ( x , y , z ) = y

trd3 :: (Integer,Integer,Integer) -> Integer
trd3 ( x , y , z ) = z

--Punto 2

{-
Definir la función aplicar, que recibe como argumento una tupla de 2 elementos con funciones y un entero, me devuelve 
como resultado una tupla con el resultado de aplicar el elemento a cada una de la funciones. 
ej: 
Main> aplicar (doble,triple) 8 
(16,24) 
Main> aplicar ((3+),(2*)) 8 
(11,16)
-}

--aplicar :: (Integer,Integer) -> Integer -> (Integer,Integer)
--aplicar ( _ , _ ) z = ( doble z , triple z )

doble :: Integer -> Integer
doble x = 2 * x

--Punto 3

{-
Definir la función cuentaBizarra, que recibe un par y: si el primer elemento es mayor al segundo devuelve la suma, si el 
segundo le lleva más de 10 al primero devuelve la resta 2do – 1ro, y si el segundo es más grande que el 1ro pero no llega 
a llevarle 10, devuelve el producto. 
Ej: 
Main> cuentaBizarra (5,8)
40
Main> cuentaBizarra (8,5)
13
Main> cuentaBizarra (5,29)
24
-}
cuentaBizarra :: (Integer, Integer) -> Integer
cuentaBizarra ( x , y ) | x > y = x + y
                        | y - x >=10 = y - x
                        | y - x < 10 = y * x


--Punto 4

{-
Representamos las notas que se sacó un alumno en dos parciales mediante un par (nota1,nota2), 
p.ej. un patito en el 1ro y un 7 en el 2do se representan mediante el par (2,7). 
A partir de esto: 
Definir la función esNotaBochazo, recibe un número y devuelve True si no llega a 6, False en caso contrario. No vale usar guardas. 
Definir la función aprobo, recibe un par e indica si una persona que se sacó esas notas aprueba. Usar esNotaBochazo. 
Definir la función promociono, que indica si promocionó, para eso tiene las dos notas tienen que sumar al menos 15 y además haberse sacado al menos 7 en cada parcial. 
Escribir una consulta que dado un par indica si aprobó el primer parcial, usando esNotaBochazo y composición. 
La consulta tiene que tener esta forma (p.ej. para el par de notas (5,8)) 
Main> (... algo ...) (5,8) 
-}


esNotaBochazo' :: Integer -> Bool
esNotaBochazo' x = x > 6

esNotaBochazo :: (Integer,Integer) -> Bool
esNotaBochazo ( nota1 , nota2 ) = nota1 < 6 && nota2 < 6

aprobo :: (Integer,Integer) -> Bool
aprobo ( nota1 , nota2 ) = not ( esNotaBochazo ( nota1 , nota2) )

promociono :: (Integer,Integer) -> Bool
promociono ( nota1 , nota2 ) = nota1 + nota2 >= 15

--ghci > esNotaBochazo' ( fst (5,8) ) 

--Punto 5

{-
Definir la función esMayorDeEdad, que dada una tupla de 2 elementos (persona, edad) me devuelva True si es mayor de 21 años y False en caso contrario. 
Por Ej:.
Main> esMayorDeEdad (juan,18) 
False 
Nota: Definir la función utilizando aplicación parcial y composición.
-}

esMayorDeEdad :: (String,Integer) -> Bool
esMayorDeEdad ( _ , y ) = y > 21

--Punto 6

{-
Definir la función calcular, que recibe una tupla de 2 elementos, si el primer elemento es par lo duplica, 
sino lo deja 
como está y con el segundo elemento en caso de ser impar le suma 1 y si no deja esté último como esta. 
Main> calcular (4,5)
(8,6) 
Main> calcular (3,7)
(3,8) 
Nota: Resolverlo utilizando aplicación parcial y composición. 
-}

calcular :: (Integer,Integer) -> (Integer,Integer)
calcular ( x , y )  = (op ( even x ) x , op' ( odd y ) y )

op :: Bool -> Integer -> Integer
op b x | not b = x
       | b = doble x

op' :: Bool -> Integer -> Integer
op' b y | not b = y
        | b = y + 1

{-
Listas
Existen funciones predefinidas en el Prelude que nos permiten manejar listas por 
ej: head / tail / !! (infija, devuelve el elemento en la posición i, base 0). Ejemplos 
head [2,4,6,8] = 2 
tail [2,4,6,8] = [4,6,8] 
[2,4,6,8] !! 1 = 4 	-- base 0!! 
null [] = True 	--Indica si una lista está vacía. 
null [2,4,5] = False 
concat [[1..4],[11..13],[21,34]] = [1,2,3,4,11,12,13,21,34] -- ”aplana” una lista de listas. 

Si se quiere calcular el promedio, dada una lista números y se está usando prelude puro, se puede hacer algo así: 
> sum [3,5,6] / fromInteger(toInteger(length[3,5,6])) 
4.66666666666667 
Tener en cuenta que en la notación [a..b] ni a ni b tienen por qué ser constantes, pueden ser cualquier expresión, p.ej 
[1..head [6,3,8]] 		[min (3+4) (3*4)..max (3+4) (3*4)] 
-}

--Punto 1

{-
 Definir una función que sume una lista de números. 
 Nota: Investigar sum 
-}


suma :: Num a => [a] -> a
suma xs = sum xs

--Punto 2
--Punto 3

--Punto 4

{-

Se tiene información detallada de la duración en minutos de las llamadas que se llevaron a cabo en un período determinado, discriminadas 
en horario normal y horario reducido. 

duracionLlamadas = (("horarioReducido",[20,10,25,15]),(“horarioNormal”,[10,5,8,2,9,10])). 

a) Definir la función cuandoHabloMasMinutos, devuelve en que horario se habló más cantidad de minutos, en el de tarifa normal o en 
el reducido. 

Main> cuandoHabloMasMinutos 
“horarioReducido” 

b) Definir la función cuandoHizoMasLlamadas, devuelve en que franja horaria realizó más cantidad de llamadas, en el de 
tarifa normal o en el reducido. 

Main> cuandoHizoMasLlamadas 
“horarioNormal” 

Nota: Utilizar composición en ambos casos 

-}

data DuracionLlamadas = DuracionLlamadas {etiqueta :: String , cantidadLlamada :: [Integer]}


cuandoHabloMasMinutos :: DuracionLlamadas -> DuracionLlamadas -> Bool
cuandoHabloMasMinutos (DuracionLlamadas _ cantidadLlamada) (DuracionLlamadas _ cantidadLlamada1)  = suma cantidadLlamada > suma cantidadLlamada1














