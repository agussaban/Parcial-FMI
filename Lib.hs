import Text.Show.Functions
import Data.List 
import Data.Char


------------------------------------------------
--------- Declaraciones de tipo y dato ---------
------------------------------------------------

data Persona = Persona {
  nombrePersona            :: String,
  direccion                :: String,
  dinero                   :: Int,
  comidaFavorita           :: Comida,
  cupones                  :: [Cupon]
} deriving Show


data Comida = Comida {
  nombreComida          :: String,
  costo                 :: Int,
  ingredientes          :: [String]

} deriving Show


type Cupon = (Comida -> Comida)



------------------------------------------------
-------------------- Maps ----------------------
------------------------------------------------

mapDinero  f persona = persona { dinero  = max 0 . f $ dinero persona }
restarDinero cantidad  persona = mapDinero (subtract cantidad) persona

mapComidaFavorita  f persona = persona { comidaFavorita  = f $ comidaFavorita  persona }
reemplazarComidaFavorita comidaNueva = mapComidaFavorita (const comidaNueva) 
------------------------------------------------
--------- Creacion de datos --------------------
------------------------------------------------

paula :: Persona
paula = Persona "Paula" "Thames 1585" 3600 hamburguesaDeluxe []

damian :: Persona
damian = Persona "Damian" "Calle sin nombre 123" 1000000 revueltoDeTofu []--[vegetariano,vegano]


hamburguesaDeluxe :: Comida
hamburguesaDeluxe = Comida "Hamburguesa deluxe" 350 ["Pan","Carne","Lechuga","Tomate","Panceta","Queso","Huevo frito"]

revueltoDeTofu :: Comida
revueltoDeTofu = Comida "Revuelto de tofu casero" 300 ["Tofu", "Salsa de soja", "Pimenton", "Sal", "Pan", "Amor"]

hamburguesaSola :: Comida
hamburguesaSola = Comida "Hamburguesa" 100 ["Pan","Carne"]

------------------------------------------------
--------- Funciones centrales  -----------------
------------------------------------------------

-- Está perfecto, lo que te podría decir para retocar es probar cómo podrías hacer para utilizar 
-- composición en la primer guarda.

comprar :: Comida -> Persona -> Persona
comprar comida persona 
                        | puedeComprar comida persona = evaluarComidaComoNuevaFavorita comida (descontarComida comida persona)
                        | otherwise = persona

puedeComprar comida persona = (costo comida) <= (dinero persona)
-- A nivel expresividad quizás quedaría más coherente que la acción sea pagarComida o alguna similar, sino se entiende que se descuenta 
-- la comida en sí (es un detalle).
descontarComida comida persona = restarDinero (costo comida) persona

evaluarComidaComoNuevaFavorita comida persona 
                        | (costo comida) < 200 = reemplazarComidaFavorita comida persona
                        | otherwise = persona

                        
carritoDeCompras :: [Comida] -> Persona -> Persona
carritoDeCompras comidas persona = (restarDinero 100).foldl (.) id (map comprar comidas) $persona 
-- Te dejo otra opción con un poco menos de complejidad. Fijate que podés usar el fold con 
-- los parámetros que ya tenes sin tener que considerar una composición y una función
-- adicional. La función que querés aplicar recursivamente sobre el resultado anterior es la compra, 
-- con lo cual la podes usar como la función parámetro del fold. Le agrego un flip para que me coincidan los tipos
-- o sino le cambio el orden de los parámetros al definir la función comprar. Entonces, nuestra semilla
-- pasa a ser la persona sobre la cual se van a aplicar todas las compras. Es decir, le estamos diciendo 
-- a la persona que compre todas las comidas, en lugar de decirle, descontá todo este monto de tu billetera una vez que 
-- te lo calculo con el fold. Lo que estás cambiando es de alguna forma la logica o semantica de lo que queres hacer.
-- Con el segundo caso te evitas el map, los parámetros hardcodeados en el fold, y aprovechas los valores que ya tenes a mano.
-- Quedaría entonces la propuesta como:
-- carritoDeCompras' comidas persona = (restarDinero 100).(foldl (flip comprar) persona) $ comidas
-- Si bien tu opción funciona bien, es un poco algoritmica. La otra propuesta es un poco más funcionalosa si se quiere. 

------------------------------------------------
--------- Cupones  -----------------------------
------------------------------------------------
-- Faltan modelas los cupones pero por el type que definiste creo que ya tenés idea de cómo encararlo. 
-- En el parcial recordá que sí va a haber que modelar los casos particulares cuando se piden. 
------------------------------------------------
--------- Funcionalidades extra  ---------------
------------------------------------------------

-- Desde el type que definiste anteriormente, los cupones son Comida -> Comida. Entonces, ¿por qué aplicarCupones tendría que recibir 
-- a la persona por parámetro? De acuerdo con la semántica definida, tendría más sentido que sea sobre una comida. Después
-- le indicarás cuál, en caso de que lo necesites (como por ejemplo con la comida favorita de la persona). Pero de la forma
-- en la que está, se acopla la persona con el cupón de forma innecesaria.
comprarConCupones :: Persona -> Persona
comprarConCupones persona = comprar (aplicarCupones persona) persona  

-- Acá sigo un poco con la idea del carritoDeCompras. El fold podría aprovechar los parámetros que ya tiene, sin hardcodear
-- la función id ni la composición. Si sabés que los cupones se aplican a las comida, tu semilla podría
-- se la comida, tu función el aplicar un cupon en particular (que implícitamente seguro se pueda salvar con un ($) para no
-- tener que delegarlo en otra función más) y tu lista serían los cupones. ¿Cómo te parece que quedaría?
aplicarCupones :: Persona -> Comida
aplicarCupones persona = foldl (.) id (cupones persona) $ (comidaFavorita persona)

superComida :: [Comida] -> Comida
superComida comidas = Comida (sacarVocales.conjuntoNombres  $ comidas) (conjuntoCostos comidas) (sinRepetidos.conjuntoIngredientes $comidas)--(conjuntoCostos comidas) (conjuntoIngredientes comidas)

conjuntoNombres comidas = conjunto.obtenerNombres $ comidas
conjuntoCostos = sum.obtenerCostos 
conjuntoIngredientes comidas = conjunto.obtenerIngredientes $ comidas

obtenerNombre comida = nombreComida comida
obtenerNombres comidas = map obtenerNombre comidas

-- Esto se podría haber puesto todo en una sola función, no hace falta delegar el "ingredientes" en otra función, porque 
-- el atributo del data en esa sintaxis ya es una función de por si. Los mismo para obtenerNombres.
obtenerIngredientes = map obtenerIngrediente
obtenerIngrediente = ingredientes

obtenerCostos = map costo

-- Está bien, creo que hay una forma más simple de encararlo con una función como concatMap, que le decis que obtenga el nombre y lo concatene
-- directo en una única lista. Es una herramienta de haskell más acorde para no tener que pensar en la recursividad del fold, pero va. 
conjunto palabras = foldl (++) [] palabras

sacarVocales = filter esVocal

esVocal letra = letra /= 'a' &&  letra /= 'e'  && letra /= 'i'  && letra /= 'o' && letra /= 'u'
-- El esVocal se podría simplificar, comprimiendolo para que chequee sobre una lista.
-- Quedaría algo como...
-- esVocal unaLetra = elem unaLetra "aeiouAEIOU" 
-- De esa forma aprovechás las herramientas que ya te da haskell con las funciones predefinidas. 

-- Esto de sinRepetidos se que lo tenes corregido así que lo contamos como bien.
sinRepetidos (x:xs) 
                    | any (==x) xs = sinRepetidos xs
                    | otherwise = x : sinRepetidos xs 

compraDeluxe ::  [Comida] -> Persona  -> Persona
compraDeluxe comidas = comprar.superComida $ (filter baratas $ comidas)

baratas comida = (costo comida) *2 < 400
-- Por como se redacto el enunciado me pareció entender que se filtra
-- por aquellas que son baratas y luego se duplica el precio antes de comparlas.
-- En ese caso se tendría que filtrar por una condición y antes de generar la super
-- comida se tendría que hacer un mapCosto para duplicar el precio antes de armarla.
-- Si interpreté mal entonces esta solución sería la correcta :)

-- En líneas generales creo que sólo faltaría modelar cada cupón.
-- Esto ayudaría para ver el tema del comentario que dejé sobre la 
-- función comprarConCupones. Como recomendación general te dejaría
-- el tip de escribir el tipo de todas las funciones (incluidas las que son "secundarias")
-- y de revisar el uso del fold para simplificar un poco esa idea. 