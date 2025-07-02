module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- Parcial STAR WARS -- 

-- Ejercicio 1 --
--Modelar las naves espaciales mencionadas y agregar una nueva nave, con un poder especial sutilmente diferente a alguna de las anteriores, en el que se aproveche las otras implementaciones. --

type Estrategia = (Nave -> Bool)
type Poder = (Nave -> Nave)
data Nave = UnaNave {
    nombre :: String,
    durabilidad :: Number,
    escudo :: Number,
    ataque :: Number,
    poder :: Poder
} deriving (Show, Eq)

-- Modelamos con Record Syntax --
tieFighter :: Nave
tieFighter = UnaNave {
    nombre = "TIE Fighter",
    durabilidad = 200,
    escudo = 100,
    ataque = 50,
    poder = turbo
}
turbo :: Poder
turbo nave = sumarAtaque 25 nave

sumarAtaque :: Number -> Nave -> Nave
sumarAtaque numero nave = nave {ataque = ((+numero).ataque)  nave}

--modifAtaque :: (Number -> Number) -> Nave -> Nave
--modifAtaque func nave = nave {ataque = (func.ataque) nave}

xWing :: Nave
xWing = UnaNave {
    nombre = "X Wing",
    durabilidad = 300,
    escudo = 150,
    ataque = 100,
    poder = reparacionDeEmergencia
}

reparacionDeEmergencia :: Poder
reparacionDeEmergencia = sumarAtaque (-30)

-- Modelamos sin Record Syntax --
naveDarthVader :: Nave
naveDarthVader = UnaNave "Nave De Darth Vader" 500 300 200 superTurbo

millenniumFalcon :: Nave
millenniumFalcon = UnaNave "Millennium Falcon" 1000 500 50 (reparacionDeEmergencia . incrementoEscudo)

superTurbo :: Poder
superTurbo nave = nave {ataque = ataque nave + 25 * 3, durabilidad = durabilidad nave - 45}

incrementoEscudo :: Poder
incrementoEscudo nave = nave {escudo = escudo nave + 100}

ironBreastplate :: Nave
ironBreastplate = UnaNave "Iron Breastplate" 450 150 125 (superTurbo . reparacionDeEmergencia)

--Que forma es mejor? Existe alguna mejor? O más expresiva/declarativa?

-- Ejercicio 2 --
--Calcular la durabilidad total de una flota, formada por un conjunto de naves, que es la suma de la durabilidad de todas las naves que la integran. --

durabilidadDeFlota :: [Nave] -> Number
durabilidadDeFlota = sum . durabilidadesDeNaves

durabilidadesDeNaves :: [Nave] -> [Number]
durabilidadesDeNaves = map durabilidad

-- Ejercicio 3 --
--Saber cómo queda una nave luego de ser atacada por otra. Cuando ocurre un ataque ambas naves primero activan su poder especial y luego la nave atacada reduce su durabilidad según el daño recibido, que es la diferencia entre el ataque de la atacante y el escudo de la atacada. (si el escudo es superior al ataque, la nave atacada no es afectada). La durabilidad, el escudo y el ataque nunca pueden ser negativos, a lo sumo 0.

ataqueEntreNaves :: Nave -> Nave -> Nave
ataqueEntreNaves atacante atacada = (reducirDurabilidad . activacionPoderes atacante) atacada
--Esto funciona si "anido" las funciones, con composición da error. Por qué ocurre eso?

reducirDurabilidad :: [Nave] -> Nave
reducirDurabilidad (atacada:atacante:_) = atacada {durabilidad = max 0 (durabilidad atacada - ataque atacante)}


activacionPoderes :: Nave -> Nave -> [Nave]
activacionPoderes atacante atacada = [activarPoderNave atacada, activarPoderNave atacante]

activarPoderNave :: Nave -> Nave
activarPoderNave nave = poderDeLaNave nave nave

poderDeLaNave :: Nave -> Poder
poderDeLaNave = poder

--Dejo los paréntesis para ayudarme a entender que primero obtengo el poder y luego lo aplico a la nave. Se puede hacer mejor? Consultar o esperar al feedback

-- Ejercicio 4 --
--Averiguar si una nave está fuera de combate, lo que se obtiene cuando su durabilidad llegó a 0. 

estaFueraDeCombate :: Nave -> Bool
estaFueraDeCombate nave =  ((== 0) .  durabilidad) nave

--Esto está raro, quizas es algo poco declarativo?? Seguro puede mejorarse

-- Ejercicio 5 --
--Averiguar cómo queda una flota enemiga luego de realizar una misión sorpresa con una nave siguiendo una estrategia. Una estrategia es una condición por la cual la nave atacante decide atacar o no una cierta nave de la flota. Por lo tanto la misión sorpresa de una nave hacia una flota significa atacar todas aquellas naves de la flota que la estrategia determine que conviene atacar. Algunas estrategias que existen, y que deben estar reflejadas en la solución, son:
--1. Naves débiles: Son aquellas naves que tienen menos de 200 de escudo.
--2. Naves con cierta peligrosidad: Son aquellas naves que tienen un ataque mayor a un valor dado. Por ejemplo, en alguna misión se podría utilizar una estrategia de peligrosidad mayor a 300, y en otra una estrategia de peligrosidad mayor a 100.
--3. Naves que quedarían fuera de combate: Son aquellas naves de la flota que luego del ataque de la nave atacante quedan fuera de combate. 
--4. Inventar una nueva estrategia

misionSorpresa :: Estrategia -> Nave -> [Nave] -> [Nave]
misionSorpresa navesDebiles atacante atacadas = [foldl ataqueEntreNaves atacante (filter navesDebiles atacadas)]

navesDebiles :: Estrategia
navesDebiles = (<200) . escudo

nivelPeligrosidadDeNave :: Number -> Estrategia
nivelPeligrosidadDeNave valorPeligrosidad = (>valorPeligrosidad) . ataque

quedanFueraDeCombate :: Nave -> Estrategia
quedanFueraDeCombate atacante atacada = (estaFueraDeCombate . ataqueEntreNaves atacante) atacada
--Nuevamente, funciona, pero no puedo aplicar composición. Será por la funcion ataqueEntreNaves? Enfectivamente, lo era

durabilidadFundida :: Number -> Nave -> Estrategia  --Despues de un ataque queda con la durabilidad menor a un valor X
durabilidadFundida valorDurabilidad atacante = (<valorDurabilidad) . durabilidad . ataqueEntreNaves atacante

-- Ejercicio 6 --
--Considerando una nave y una flota enemiga en particular, dadas dos estrategias, determinar cuál de ellas es la que minimiza la durabilidad total de la flota atacada y llevar adelante una misión con ella.


mejorEstrat :: Estrategia -> Estrategia -> Nave -> [Nave] -> Number
mejorEstrat estrat1 estrat2 atacante atacadas = min (durabilidadEstrat estrat1 atacante atacadas) (durabilidadEstrat estrat2 atacante atacadas)

durabilidadEstrat :: Estrategia -> Nave -> [Nave] -> Number
durabilidadEstrat estrategia atacante atacadas = (durabilidadDeFlota . misionSorpresa estrategia atacante) atacadas

-- Ejercicio 7 --
--Construir una flota infinita de naves. ¿Es posible determinar su durabilidad total? ¿Qué se obtiene como respuesta cuando se lleva adelante una misión sobre ella? Justificar conceptualmente.
-- Respuestas:
-- No es posible determinar su durabilidad total, ya que al ser una suma constante, nunca convergería a un valor específico. Por ende, el lenguaje estaría sumando infinitamente.
-- Para esta pregunta hay 2 posibles resultados, el primero, si una cantidad finita de naves son las que cumplen la estrategia, la misión se hará sin problemas y nos dará como quedan las naves luego de ella. Ahora, si la totalidad de la flota, son naves que no cumplen la estrategia, el lenguaje estrá evaluando infinitamente hasta acabar la lista (o sea, nunca).



---------------------------------------------------------------------------

-- CONSULTAS A LOS PROFES: --

-- 1) Porqué esto está mal? (me da error)

--sumatoria :: [Number] -> Number
--sumatoria numeros = sum . filter even numeros

-- Pero con esto está bien? Quiźas por agrupar la composición?

--sumatoria :: [Number] -> Number
--sumatoria numeros = (sum . filter even) numeros

--La respuesta, debido a lo que hablé con Pedro, el primero está mal, porque la composición, recibe 1 solo parámetro, en el primer ejemplo, sin los paréntesis, le estoy pasando 2 parámetros, even y numero; en cambio, en el segundo ejemplo, yo agrupo, uno de los parámetros con los paréntesis, y luego le paso 1 solo parámetro, a la composición. 
----------------------------------------------------------

-- Esto es una solución declarativa y expresiva. 
-- Declarativa: Menos detalle algortimico ;
-- Expresiva: Es un código legible y entendible para mí ;

--sumatoriaPares :: [Number] -> Number
--sumatoriaPares = sum . numerosPares 

--numerosPares :: [Number] -> [Number]
--numerosPares = filter even

--sumatoriaImpares :: [Number] -> Number
--sumatoriaImpares = sum . numerosImpares

--numerosImpares :: [Number] -> [Number]
--numerosImpares = filter (not . even)