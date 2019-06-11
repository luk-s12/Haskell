module Library where

import PdePreludat
import Data.List (isPrefixOf)

type Ingrediente   = String
type GradosCelsius = Int
type Gusto         = String

type Dispenser    = Int
type CajonDeFruta = String
type Maquina      = (Helado -> Helado)


data Helado = Helado{
    gusto :: Gusto,
    temperatura :: GradosCelsius,
    ingredientes :: [Ingrediente]
}deriving (Show)

{-
                    ██████╗ ██╗   ██╗███╗   ██╗████████╗ ██████╗      ██╗
                    ██╔══██╗██║   ██║████╗  ██║╚══██╔══╝██╔═══██╗    ███║
                    ██████╔╝██║   ██║██╔██╗ ██║   ██║   ██║   ██║    ╚██║
                    ██╔═══╝ ██║   ██║██║╚██╗██║   ██║   ██║   ██║     ██║
                    ██║     ╚██████╔╝██║ ╚████║   ██║   ╚██████╔╝     ██║
                    ╚═╝      ╚═════╝ ╚═╝  ╚═══╝   ╚═╝    ╚═════╝      ╚═╝
-}


tieneBuenaTemperatura :: GradosCelsius -> Helado -> Bool
tieneBuenaTemperatura grados = (<=grados).temperatura 

empiezaConChocolate :: Helado ->Bool
empiezaConChocolate = isPrefixOf "chocolate".gusto

tieneAgua :: Helado -> Bool
tieneAgua = elem "agua".ingredientes

mismosGradosPorIngredientes :: GradosCelsius -> Helado -> Bool 
mismosGradosPorIngredientes grados = (grados<=).negate.cantidadDeIngrediente

cantidadDeIngrediente :: Helado -> Int
cantidadDeIngrediente = length.ingredientes

estaBienPreparado :: Helado -> Bool
estaBienPreparado helado 
 | empiezaConChocolate helado = tieneBuenaTemperatura (-10) helado
 | tieneAgua helado = tieneBuenaTemperatura (-5) helado
 | otherwise = mismosGradosPorIngredientes (temperatura helado) helado


{-
                    ██████╗ ██╗   ██╗███╗   ██╗████████╗ ██████╗     ██████╗ 
                    ██╔══██╗██║   ██║████╗  ██║╚══██╔══╝██╔═══██╗    ╚════██╗
                    ██████╔╝██║   ██║██╔██╗ ██║   ██║   ██║   ██║     █████╔╝
                    ██╔═══╝ ██║   ██║██║╚██╗██║   ██║   ██║   ██║    ██╔═══╝ 
                    ██║     ╚██████╔╝██║ ╚████║   ██║   ╚██████╔╝    ███████╗
                    ╚═╝      ╚═════╝ ╚═╝  ╚═══╝   ╚═╝    ╚═════╝     ╚══════╝
-}
 
heladera :: GradosCelsius ->  Maquina
heladera grado helado = helado {temperatura = temperatura helado +  grado}  

--cambio de nombre a agregadora
agregadora :: Ingrediente ->  Maquina
agregadora ingrediente helado = helado {ingredientes = agrega (ingredientes helado) [ingrediente]}

agrega :: [a] -> [a] -> [a]
agrega ingredientes otrosIngredientes = ingredientes ++ otrosIngredientes 

mixturadora :: Helado -> Maquina
mixturadora helado otroHelado = Helado{ 
    gusto =  agrega (gusto helado) (" y " ++ gusto otroHelado),
    temperatura = menorTemperatura (temperatura helado) (temperatura otroHelado),
    ingredientes = nuevosIngredientes (ingredientes helado) (ingredientes otroHelado)
}

menorTemperatura :: GradosCelsius -> GradosCelsius -> GradosCelsius
menorTemperatura temperatura otraTemperatura = min temperatura otraTemperatura

nuevosIngredientes :: [Ingrediente] -> [Ingrediente] -> [Ingrediente]
nuevosIngredientes ingredientes  = sinRepetidos.agrega ingredientes 

sinRepetidos :: [Ingrediente] -> [Ingrediente]
sinRepetidos [] = []
sinRepetidos (x:xs) = x : sinRepetidos (filter (/= x) xs)

batidor :: CajonDeFruta -> Dispenser -> Maquina
batidor fruta temperaturaDeldispenser helado = Helado{
    gusto = fruta,
    temperatura = temperaturaDeldispenser, 
    ingredientes = ["agua",fruta] 
}

choripastear :: Maquina
choripastear helado = helado{
    gusto = agrega (gusto helado) " de la casa",
    ingredientes = (sinRepetidos.agrega ["agua","esencia artificial"].ingredientes) helado
}

heladoNeutro = Helado{
  gusto = "",
  temperatura = 0,
  ingredientes =[]
}
{-
Mediante el uso de composición, mostrar cómo podría tener un helado neutro, batirlo con
frutilla y un dispenser de -5 grados, pasarlo por una heladera que le baje 5 grados, y agregarle
azúcar. Explique dónde aparece el concepto de aplicación parcial.

Esta resuelto en los test
-}

{-
                    ██████╗ ██╗   ██╗███╗   ██╗████████╗ ██████╗     ██████╗ 
                    ██╔══██╗██║   ██║████╗  ██║╚══██╔══╝██╔═══██╗    ╚════██╗
                    ██████╔╝██║   ██║██╔██╗ ██║   ██║   ██║   ██║     █████╔╝
                    ██╔═══╝ ██║   ██║██║╚██╗██║   ██║   ██║   ██║     ╚═══██╗
                    ██║     ╚██████╔╝██║ ╚████║   ██║   ╚██████╔╝    ██████╔╝
                    ╚═╝      ╚═════╝ ╚═╝  ╚═══╝   ╚═╝    ╚═════╝     ╚═════╝ 
-}


chocolateInfinito = Helado{
  gusto = "chocolate infinito",
  temperatura = -4,
  ingredientes = ["azucar","leche"] ++ repeat "chocolate"
}
-- ambas respuestas de las preguntas de este punto se encuentran en los test

{-
                    ██████╗ ██╗   ██╗███╗   ██╗████████╗ ██████╗     ██╗  ██╗
                    ██╔══██╗██║   ██║████╗  ██║╚══██╔══╝██╔═══██╗    ██║  ██║
                    ██████╔╝██║   ██║██╔██╗ ██║   ██║   ██║   ██║    ███████║
                    ██╔═══╝ ██║   ██║██║╚██╗██║   ██║   ██║   ██║    ╚════██║
                    ██║     ╚██████╔╝██║ ╚████║   ██║   ╚██████╔╝         ██║
                    ╚═╝      ╚═════╝ ╚═╝  ╚═══╝   ╚═╝    ╚═════╝          ╚═╝
-}

cintaTransportadora :: [Maquina] -> Helado
cintaTransportadora  = foldl (\helado ->(\ maquina -> maquina helado) ) heladoNeutro

{-
Otra forma de hacerlo es: 

foldr ($) heladoNeutro.reverse

-}

maquinasQuePreparanBien :: Helado -> [Maquina] -> [Maquina]
maquinasQuePreparanBien helado = filter (\ maquina -> (estaBienPreparado.maquina ) helado)
{-


                    ██████╗  ██████╗ ███╗   ██╗██╗   ██╗███████╗
                    ██╔══██╗██╔═══██╗████╗  ██║██║   ██║██╔════╝
                    ██████╔╝██║   ██║██╔██╗ ██║██║   ██║███████╗
                    ██╔══██╗██║   ██║██║╚██╗██║██║   ██║╚════██║
                    ██████╔╝╚██████╔╝██║ ╚████║╚██████╔╝███████║
                    ╚═════╝  ╚═════╝ ╚═╝  ╚═══╝ ╚═════╝ ╚══════╝                                              
-}

ingredienteFavorito :: [Helado] -> String
ingredienteFavorito helados = 
    (obtenerElIngrediente.
    segunLaCantidadMaxima (ingredienteYCantidad helados) .
    cantidadMaximaDelIngrediente
    )helados
    
cantidadMaximaDelIngrediente = maximum.cantidadDeCadaIngrediente 

cantidadDeCadaIngrediente helados = 
    (cantidades .
    juntarSegunElIngrediente (todosLosingredientes helados).
    ingredienteSinRepetidos
    ) helados  

juntarSegunElIngrediente ingredientes = map (\elemento -> filter (== elemento) ingredientes )  

cantidades  = map length

ingredienteSinRepetidos = sinRepetidos.concat.map ingredientes

todosLosingredientes = concat.map ingredientes

ingredienteYCantidad  helados = zipWith (,) (ingredienteSinRepetidos helados) (cantidadDeCadaIngrediente helados) 

segunLaCantidadMaxima ingredientesConCantidades mayorCantidad  = (filter (\ ( _, cantidad) -> cantidad == mayorCantidad ) ) ingredientesConCantidades

obtenerElIngrediente =  concat.map fst

