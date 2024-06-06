module Library where
import PdePreludat

type Tolerancia = Number
type Titulo = String
type Reconocimientos = Number

--Los Fremen
data Fremen = UnFremen {
    nombre :: String,
    tolerancia :: Tolerancia,
    titulos :: [Titulo],
    reconocimientos :: Reconocimientos
} deriving (Show, Eq)

type Tribu = [Fremen]

--Punto A
recibirReconocimiento :: Fremen -> Fremen
recibirReconocimiento fremen = fremen {reconocimientos = reconocimientos fremen + 1}

--Punto B
existeElegido :: Tribu -> Bool
existeElegido = any candidatoAElegido

candidatoAElegido :: Fremen -> Bool
candidatoAElegido fremen = tieneTitulo "Domador" fremen && tolerancia fremen > 100

--Punto C
--Hipotesis: no hay tribus vacias, y siempre hay un elegido
hallarElElegido :: Tribu -> Fremen
hallarElElegido [fremen] = fremen
hallarElElegido tribu = foldl1 tieneMasReconocimientos (filter candidatoAElegido tribu)

tieneMasReconocimientos :: Fremen -> Fremen -> Fremen
tieneMasReconocimientos candidato1 candidato2
    | reconocimientos candidato1 > reconocimientos candidato2 = candidato1
    | otherwise = candidato2

--Gusanos de arena

data Gusano = UnGusano {
    longitud :: Number,
    nivelDeHidratacion :: Number,
    descripcion :: String
} deriving (Show, Eq)

reproduccionGusanos :: Gusano -> Gusano -> Gusano
reproduccionGusanos gusano1 gusano2 =
    UnGusano {longitud = longitudCria gusano1 gusano2, nivelDeHidratacion = 0, descripcion = concatenarDescrpciones gusano1 gusano2}

longitudCria :: Gusano -> Gusano -> Number
longitudCria gusano1 gusano2 = 10 * max (longitud gusano1) (longitud gusano2) / 100

concatenarDescrpciones :: Gusano -> Gusano -> String
concatenarDescrpciones gusano1 gusano2 = descripcion gusano1 ++ " - " ++ descripcion gusano2

--Hipotesis: Las listas no estan vacias y son 1 a 1
nuevasCrias :: [Gusano] -> [Gusano] -> [Gusano]
nuevasCrias listaGusanos1 listaGusanos2 = map reproduccionGusanosconTupla (zip listaGusanos1 listaGusanos2)

reproduccionGusanosconTupla :: (Gusano, Gusano) -> Gusano
reproduccionGusanosconTupla (gusano1, gusano2) = reproduccionGusanos gusano1 gusano2

--Misiones


data Mision = UnaMision {
    condicion :: Fremen -> Gusano -> Bool,
    resultado :: Gusano -> Fremen-> Fremen
}


domarGusano :: Mision
domarGusano = UnaMision condicionDomarGusano intentarDomarGusano
condicionDomarGusano :: Fremen -> Gusano -> Bool
condicionDomarGusano fremen gusano = tolerancia fremen > (longitud gusano / 2)
intentarDomarGusano :: Gusano -> Fremen -> Fremen
intentarDomarGusano gusano fremen
    | condicionDomarGusano fremen gusano = (aumentarTolerancia 100 . otorgarTitulo "Domador") fremen
    | otherwise = modificarToleranciaPorcentual 0.9 fremen

destruirGusano :: Mision
destruirGusano = UnaMision condicionDestruirGusano intentarDestruirGusano
condicionDestruirGusano :: Fremen -> Gusano -> Bool
condicionDestruirGusano fremen gusano = tieneTitulo "Domador" fremen && tolerancia fremen < (longitud gusano / 2)
intentarDestruirGusano :: Gusano -> Fremen -> Fremen
intentarDestruirGusano gusano fremen
    | condicionDestruirGusano fremen gusano = (recibirReconocimiento . aumentarTolerancia 100) fremen
    |otherwise = modificarToleranciaPorcentual 0.8 fremen

tieneTitulo :: String -> Fremen -> Bool
tieneTitulo titulo fremen = titulo `elem` titulos fremen

aumentarTolerancia :: Number -> Fremen -> Fremen
aumentarTolerancia puntosTolerancia fremen = fremen {tolerancia = tolerancia fremen + puntosTolerancia}

modificarToleranciaPorcentual :: Number -> Fremen -> Fremen
modificarToleranciaPorcentual porcentaje fremen = fremen {tolerancia = tolerancia fremen * porcentaje}

otorgarTitulo :: String -> Fremen -> Fremen
otorgarTitulo titulo fremen = fremen {titulos = titulo : titulos fremen}


realizacionColectivaDeMision :: Tribu -> Mision -> Gusano -> Tribu
realizacionColectivaDeMision tribu mision gusano = map (completarMision mision gusano) tribu

completarMision :: Mision -> Gusano -> Fremen -> Fremen
completarMision mision gusano fremen
    | condicion mision fremen gusano = resultado mision gusano fremen
    | otherwise = fremen


distintoElegido :: Tribu -> Mision -> Gusano -> Bool
distintoElegido tribu mision gusano = hallarElElegido tribu /= hallarElElegido (realizacionColectivaDeMision tribu mision gusano)


--Al infinito

{-Si la funcion de entrenarlos me devuelve al freemen entrenado, esta nunca devolveria un valor. Si se usa map en la consola te devuelve valores
a medida que los entrena pero en algun momento rompe.

la funcion recorre hasta encontrar un elegido, ahi corta y devuelve True, en caso de no haber elegidos, va a seguir recorriendo la lista hasta romper

no lo encontraria porque compara entre todos los fremen de una tribu, siempre va a querer comparar con el siguiente mas alla de que "lo haya encontrado",
(queriendo decir que nosotros sabemos cual es el elegido y ya se quedo con ese pero igual va a seguir comparando hasta el final)
-}