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

data Parametros = Parametros {nivel :: (Integer,Integer,Integer)}

dispersion :: Parametros -> (Integer , String)
dispersion Parametros ( x , y , z)  =  (diferenciaMaxMin Parametros , diasNormales Parametros)

diferenciaMaxMin :: Parametros -> Integer
diferenciaMaxMin Parametros  = max (max x y) z - min ( min x y) z

diasNormales :: Parametros -> String
diasNormales | (<30). diferenciaMaxMin Parametros = 

























