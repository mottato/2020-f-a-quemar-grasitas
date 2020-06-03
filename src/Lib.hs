module Lib where
import Text.Show.Functions

data Gimnasta = Gimnasta{
    nombre :: String,
    edad:: Float,
    peso:: Float,
    coeficienteTonificacion :: Float
} deriving(Show,Eq)

pancho = Gimnasta "Francisco" 40.0 120.0 1.0
andres = Gimnasta "Andy" 22.0 80.0 6.0

--1)

saludable :: Gimnasta->Bool
saludable unGimnasta = noEstaObeso unGimnasta && tieneCoeficienteDeTonificacionMayorA5 unGimnasta

noEstaObeso :: Gimnasta->Bool
noEstaObeso =(<100).peso

tieneCoeficienteDeTonificacionMayorA5 :: Gimnasta->Bool
tieneCoeficienteDeTonificacionMayorA5 = (>5).coeficienteTonificacion

--2)

quemarCalorias :: Float->Gimnasta->Gimnasta
quemarCalorias unasCalorias unGimnasta
 |not.noEstaObeso $unGimnasta                                               = disminuirCalorias unasCalorias (/150) unGimnasta
 |noEstaObeso unGimnasta && ((>30).edad $unGimnasta) && (unasCalorias > 200)= disminuirCalorias unasCalorias (+1) unGimnasta
 |otherwise                                                                 = disminuirCalorias unasCalorias (/(productoPesoYEdad unGimnasta)) unGimnasta

disminuirCalorias ::Float ->(Float->Float)->Gimnasta->Gimnasta
disminuirCalorias cantidadCalorias unaFuncion unGimnasta = unGimnasta {
    peso = (peso unGimnasta) - (unaFuncion cantidadCalorias)
}

productoPesoYEdad :: Gimnasta->Float
productoPesoYEdad unGimnasta= (peso unGimnasta)*(edad unGimnasta)

--3)

type Ejercicio = Float->Gimnasta->Gimnasta

caminataEnCinta :: Ejercicio
caminataEnCinta tiempo unGimnasta = quemarCalorias (caloriasCinta 5 tiempo) unGimnasta


entrenamientoEnCinta :: Ejercicio 
entrenamientoEnCinta tiempo unGimnasta = quemarCalorias (caloriasCinta (6+(tiempo/5)/2) tiempo) unGimnasta

caloriasCinta:: Float->Float->Float
caloriasCinta velocidad tiempo = tiempo*velocidad
 
pesas :: Float->Ejercicio
pesas kilosALevantar tiempo unGimnasta
 |tiempo >10 = tonificar kilosALevantar (/10) unGimnasta
 |otherwise  = unGimnasta

tonificar :: Float ->(Float->Float)->Gimnasta->Gimnasta
tonificar cantidad unaFuncion unGimnasta = unGimnasta{
    coeficienteTonificacion = (coeficienteTonificacion unGimnasta) + (unaFuncion cantidad)
}

colina :: Float->Ejercicio
colina inclinacion tiempo unGimnasta = quemarCalorias (2*inclinacion*tiempo) unGimnasta

montania :: Float->Ejercicio
montania inclinacion tiempo = (tonificar 1 (\cantidad->1)).(quemarCalorias (caloriasMontania inclinacion tiempo))

caloriasMontania :: Float->Float->Float
caloriasMontania inclinacion tiempo = 2*(tiempo/2)*inclinacion + 2*(tiempo/2)*(inclinacion + 3)

--4)

data Rutina = Rutina {
    nombreRutina :: String,
    duracionTotal :: Float,
    ejercicios :: [Ejercicio]
} deriving(Show)



