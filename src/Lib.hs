module Lib where
import Text.Show.Functions

data Gimnasta = Gimnasta{
    nombre :: String,
    edad:: Int,
    peso:: Int,
    coeficienteTonificacion :: Int
} deriving(Show,Eq)

pancho = Gimnasta "Francisco" 40 120 1
andres = Gimnasta "Andy" 22 80 6

--1)

saludable :: Gimnasta->Bool
saludable unGimnasta = noEstaObeso unGimnasta && tieneCoeficienteDeTonificacionMayorA5 unGimnasta

noEstaObeso :: Gimnasta->Bool
noEstaObeso =(<100).peso

tieneCoeficienteDeTonificacionMayorA5 :: Gimnasta->Bool
tieneCoeficienteDeTonificacionMayorA5 = (>5).coeficienteTonificacion

--2)

quemarCalorias :: Int->Gimnasta->Gimnasta
quemarCalorias unasCalorias unGimnasta
 |not.noEstaObeso $unGimnasta                                               = disminuirCalorias unasCalorias (`div` 150) unGimnasta
 |noEstaObeso unGimnasta && ((>30).edad $unGimnasta) && (unasCalorias > 200)= disminuirCalorias unasCalorias (+1) unGimnasta
 |otherwise                                                                 = disminuirCalorias unasCalorias (`div` (productoPesoYEdad unGimnasta)) unGimnasta

disminuirCalorias ::Int ->(Int->Int)->Gimnasta->Gimnasta
disminuirCalorias cantidadCalorias unaFuncion unGimnasta = unGimnasta {
    peso = (peso unGimnasta) - (unaFuncion cantidadCalorias)
}

productoPesoYEdad :: Gimnasta->Int
productoPesoYEdad unGimnasta= (peso unGimnasta)*(edad unGimnasta)

--3)

type Ejercicio = Int->Gimnasta->Gimnasta

caminataEnCinta :: Ejercicio
caminataEnCinta tiempo unGimnasta = quemarCalorias (caloriasCinta 5 tiempo) unGimnasta


entrenamientoEnCinta :: Ejercicio 
entrenamientoEnCinta tiempo unGimnasta = quemarCalorias (caloriasCinta (6+(div tiempo 5) `div`2) tiempo) unGimnasta

caloriasCinta:: Int->Int->Int
caloriasCinta velocidad tiempo = tiempo*velocidad
 
pesas :: Int->Ejercicio
pesas kilosALevantar tiempo unGimnasta
 |tiempo >10 = tonificar kilosALevantar (`div`10) unGimnasta
 |otherwise  = unGimnasta

tonificar :: Int ->(Int->Int)->Gimnasta->Gimnasta
tonificar cantidad unaFuncion unGimnasta = unGimnasta{
    coeficienteTonificacion = (coeficienteTonificacion unGimnasta) + (unaFuncion cantidad)
}

colina :: Int->Ejercicio
colina inclinacion tiempo unGimnasta = quemarCalorias (2*inclinacion*tiempo) unGimnasta

montania :: Int->Ejercicio
montania inclinacion tiempo = (tonificar 1 (\cantidad->1)).(quemarCalorias (caloriasMontania inclinacion tiempo))

caloriasMontania :: Int->Int->Int
caloriasMontania inclinacion tiempo = 2*(tiempo `div` 2)*inclinacion + 2*(tiempo `div` 2)*(inclinacion + 3)

--4)

data Rutina = Rutina {
    nombreRutina :: String,
    duracionTotal :: Int,
    ejercicios :: [Ejercicio]
} deriving(Show)



duracionPorEjercicio :: Int->[Ejercicio]->Int
duracionPorEjercicio duracionTotal = (div duracionTotal).length

--Con recursividad 
realizarRutina :: Rutina->Gimnasta->Gimnasta
realizarRutina unaRutina unGimnasta= ejercitar (duracionPorEjercicio (duracionTotal unaRutina) (ejercicios unaRutina)) (ejercicios unaRutina) unGimnasta

ejercitar :: Int->[Ejercicio]->Gimnasta->Gimnasta
ejercitar _ [] unGimnasta = unGimnasta
ejercitar tiempo (ej:ejs) unGimnasta = (ejercitar tiempo ejs) (ej tiempo unGimnasta)

--Con foldr

realizarRutinaV2 :: Rutina->Gimnasta->Gimnasta
realizarRutinaV2 unaRutina unGimnasta = foldr ($) unGimnasta (aplicarARutina unaRutina)

aplicarARutina :: Rutina->[Gimnasta->Gimnasta]
aplicarARutina (Rutina _ duracionTotal ejercicios)= map (\ej-> ej (duracionPorEjercicio duracionTotal ejercicios)) ejercicios

rutina = Rutina "Micardi" 10 [caminataEnCinta ,pesas 50]

-- aplicarRutina rutina pancho


puedenLlevarASaludable :: [Rutina]->Gimnasta->[Rutina]
puedenLlevarASaludable listaRutinas unGimnasta= filter (\rutina -> saludable (realizarRutina rutina unGimnasta))  listaRutinas