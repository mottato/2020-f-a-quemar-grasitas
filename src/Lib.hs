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

quemarCalorias :: Gimnasta->Float->Gimnasta
quemarCalorias unGimnasta unasCalorias
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
caminataEnCinta tiempo unGimnasta = quemarCalorias unGimnasta (caloriasCinta 5 tiempo)


entrenamientoEnCinta :: Ejercicio 
entrenamientoEnCinta tiempo unGimnasta = quemarCalorias unGimnasta (caloriasCinta (6+(tiempo/5)/2) tiempo)

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
colina inclinacion tiempo unGimnasta = quemarCalorias unGimnasta (2*inclinacion*tiempo)