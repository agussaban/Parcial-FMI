data Pais = Pais {
    ingresoPerCapita             :: Float,
    poblacionActivaSectorPublico :: Int,
    poblacionActivaSectorPrivado :: Int,
    recursosNaturales            :: [Recurso],
    deuda                        :: Float
} deriving (Show, Eq)

type Recurso = String

--Auxiliares

mapIngresoPerCapita :: (Float -> Float) -> Pais -> Pais
mapIngresoPerCapita funcion pais = pais { ingresoPerCapita = funcion . ingresoPerCapita $ pais }

mapPoblacionActivaSectorPublico :: (Int -> Int) -> Pais -> Pais
mapPoblacionActivaSectorPublico funcion pais = pais { poblacionActivaSectorPublico = funcion . poblacionActivaSectorPublico $ pais }

mapPoblacionActivaSectorPrivado :: (Int -> Int) -> Pais -> Pais
mapPoblacionActivaSectorPrivado funcion pais = pais { poblacionActivaSectorPrivado = funcion . poblacionActivaSectorPrivado $ pais }

mapRecursosNaturales :: ([Recurso] -> [Recurso]) -> Pais -> Pais
mapRecursosNaturales funcion pais = pais { recursosNaturales = funcion . recursosNaturales $ pais }

mapDeuda :: (Float -> Float) -> Pais -> Pais
mapDeuda funcion pais = pais { deuda = funcion . deuda $ pais }


{-
Dar un ejemplo de cómo generar al país Namibia, cuyo ingreso per cápita es de 4140 u$s, 
la población activa del sector público es de 400.000, la población activa del sector privado es de 650.000, 
su riqueza es la minería y el ecoturismo y le debe 50 (millones de u$s) al FMI.
-}

namibia :: Pais
namibia = Pais 4140 400000 650000 ["mineria", "ecoturismo"] 50

francia :: Pais
francia = Pais 4140 2656566 22323030 ["mineria", "ecoturismo"] 92


-- PUNTO 2
type Estrategia = Pais -> Pais

-- prestarle n millones de dólares al país, esto provoca que el país se endeude en un 150% de lo que el FMI le presta (por los intereses)

prestarPlata :: Float -> Estrategia
prestarPlata cuanto pais = pais { deuda = deuda pais + cobrarIntereses cuanto }

cobrarIntereses :: Float -> Float
cobrarIntereses cuanto = cuanto * 1.5 


-- reducir x cantidad de puestos de trabajo del sector público, lo que provoca que se reduzca la cantidad de activos en el sector público 
-- y además que el ingreso per cápita disminuya en 20% si los puestos de trabajo son más de 100 ó 15% en caso contrario

reducirPuestos :: Int -> Estrategia
reducirPuestos cuantos pais = pais {
    poblacionActivaSectorPublico = poblacionActivaSectorPublico pais - cuantos,
    ingresoPerCapita = ingresoPerCapita pais - (1 - reduccionIngresos cuantos)
 }

reduccionIngresos :: Int -> Float
reduccionIngresos cantidadPuestos 
 | cantidadPuestos > 100 = 0.2
 | otherwise             = 0.15

-- darle a una empresa afín al FMI la explotación de alguno de los recursos naturales, esto disminuye 2 millones de dólares 
-- la deuda que el país mantiene con el FMI pero también deja momentáneamente sin recurso natural a dicho país. 
-- No considerar qué pasa si el país no tiene dicho recurso.

explotacion :: Recurso -> Estrategia
explotacion recurso pais = pais {
    recursosNaturales = quitarRecurso recurso $ recursosNaturales pais,
    deuda = deuda pais - 20
}

quitarRecurso :: Recurso -> [Recurso] -> [Recurso]
quitarRecurso recurso listaRecursos = filter (/= recurso) listaRecursos

{-
establecer un “blindaje”, lo que provoca prestarle a dicho país la mitad de su Producto Bruto Interno (que se calcula como el ingreso 
per cápita multiplicado por su población activa, sumando puestos públicos y privados de trabajo) y reducir 500 puestos de trabajo del 
sector público. Evitar la repetición de código.
-}

blindaje :: Estrategia
blindaje pais = (prestarPlata (pbi pais * 0.5) . reducirPuestos 500) pais

pbi :: Pais -> Float
pbi pais = ingresoPerCapita pais * fromIntegral (poblacionActiva pais)

poblacionActiva :: Pais -> Int
poblacionActiva pais = poblacionActivaSectorPublico pais + poblacionActivaSectorPrivado pais

-- Modelar una receta que consista en prestar 200 millones, y darle a una empresa X la explotación de la “Minería” de un país.
type Receta = [Estrategia]

receta :: Receta
receta = [prestarPlata 2000, explotacion "Mineria"]

--Ahora queremos aplicar la receta del punto 3.a al país Namibia (creado en el punto 1.b). Justificar cómo se logra el efecto colateral.
aplicarReceta :: Receta -> Pais -> Pais
aplicarReceta receta pais = foldr ($) pais receta

-- Dada una lista de países conocer cuáles son los que pueden zafar, aquellos que tienen "Petróleo" entre sus riquezas naturales.

puedenZafar :: [Pais] -> [Pais]
puedenZafar paises = (filter $ elem "Petroleo" . recursosNaturales) paises

-- Dada una lista de países, saber el total de deuda que el FMI tiene a su favor.

deudaTotal :: [Pais] -> Float
deudaTotal = foldr ((+) . deuda) 0

{-
dado un país y una lista de recetas, saber si la lista de recetas está ordenada de “peor” a “mejor”, 
en base al siguiente criterio: si aplicamos una a una cada receta, el PBI del país va de menor a mayor. 
Recordamos que el Producto Bruto Interno surge de multiplicar el ingreso per cápita por la población activa (privada y pública). 
-}

estaOrdenado :: Pais -> [Receta] -> Bool
estaOrdenado pais [receta] = True
estaOrdenado pais (receta1:receta2:recetas) 
     = revisarPBI receta1 pais <= revisarPBI receta2 pais && estaOrdenado pais (receta2:recetas)  
     where revisarPBI receta = pbi . aplicarReceta receta