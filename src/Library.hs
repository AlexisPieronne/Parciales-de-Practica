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
    nombreNave :: String,
    durabilidad :: Number,
    escudo :: Number,
    ataque :: Number,
    poder :: Poder
} deriving (Show, Eq)

-- Modelamos con Record Syntax --
tieFighter :: Nave
tieFighter = UnaNave {
    nombreNave = "TIE Fighter",
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
    nombreNave = "X Wing",
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

-- Parcial HARRY POSTRE Y EL PASTEL CURRIFICADO --

-- Ejercicio 1A --
--Modelar los postres. Un mismo postre puede tener muchos sabores, tiene un peso y se sirve a cierta temperatura. Por ejemplo, un bizcocho borracho de fruta y crema de 100 gramos servido a 25°C.

data Postre = UnPostre {
    nombrePostre :: String,
    sabores :: [String],
    pesoPostre :: Number,
    temperatura :: Number
} deriving(Show,Eq)

bizcocho :: Postre
bizcocho = UnPostre {
    nombrePostre = "Bizcocho borracho",
    sabores = ["Frutas","Crema"],
    pesoPostre = 100,
    temperatura = 25
}

torta :: Postre
torta = UnPostre {
    nombrePostre = "Torta Helada",
    sabores = ["Chocolate","Whisky"],
    pesoPostre = 75,
    temperatura = 15
}

flan :: Postre
flan = UnPostre {
    nombrePostre = "Flan Muggle",
    sabores = ["Dulce De Leche","Crema"],
    pesoPostre = 50,
    temperatura = 17
}

tarta :: Postre
tarta = UnPostre {
    nombrePostre = "Tarta",
    sabores = ["Melaza"],
    pesoPostre = 50,
    temperatura = 0
}
-- Ejercicio 1B --
--Modelar los hechizos, sabiendo que deberían poderse agregar más sin modificar el código existente. Por ahora existen los siguientes:
--  Incendio: calienta el postre 1 grado y lo hace perder 5% de su peso.
--  Immobulus: congela el postre, llevando su temperatura a 0.
--  Wingardium Leviosa: levanta el postre en el aire y lo deja caer, lo que agrega a sus sabores el sabor “concentrado”. Además, pierde 10% de su peso.
--  Diffindo: Corta el postre, disminuyendo su peso en el porcentaje indicado. 
--  Riddikulus: Requiere como información adicional un sabor y lo agrega a los sabores que tiene un postre, pero invertido.
--  Avada kedavrlistosa: Hace lo mismo que el immobulus pero además hace que el postre pierda todos sus sabores.

type Hechizos = Postre -> Postre

incendio :: Hechizos
incendio = modifPeso 5 . aumentarTemperatura 1

immobulus :: Hechizos
immobulus postre = postre {temperatura = 0}

wingardiumLeviosa :: Hechizos
wingardiumLeviosa = agregarSabor "Concentrado" . modifPeso 10

diffindo :: Number -> Hechizos
diffindo valor = modifPeso valor

riddikulus :: String -> Hechizos
riddikulus saborExtra = (agregarSabor . invertirSabor) saborExtra

avadaKedavra :: Hechizos
avadaKedavra = immobulus . sacarSabores

sacarSabores :: Postre -> Postre
sacarSabores postre = postre {sabores = []}

modifPeso :: Number -> Postre -> Postre
modifPeso valor postre = postre {pesoPostre = (subtract (porcentajeDePeso valor postre) . pesoPostre) postre}  --Se puede aplicar composición con el subtract??

porcentajeDePeso :: Number -> Postre -> Number
porcentajeDePeso valor postre = (valor * pesoPostre postre)/100

aumentarTemperatura :: Number -> Postre -> Postre
aumentarTemperatura valor postre = postre {temperatura = ((+valor) . temperatura) postre}

agregarSabor :: String -> Postre -> Postre
agregarSabor saborExtra postre = postre {sabores = ((++[saborExtra]) . sabores) postre}

invertirSabor :: String -> String
invertirSabor = reverse

-- Ejercicio 1C --
--Dado un conjunto de postres en la mesa, saber si hacerles un determinado hechizo los dejará listos (un postre está listo cuando pesa algo más que cero, tiene algún sabor y además no está congelado).
--Por ejemplo, si en la mesa está el bizcocho mencionado anteriormente y una tarta de melaza de 0 grados y 50 gramos, y les hago el hechizo incendio, quedan listos, pero si les hago el hechizo riddikulus con el sabor “nomil” no, porque la tarta sigue congelada.  

estanPostresListo :: Hechizos -> [Postre] -> Bool
estanPostresListo hechizo postres = (validacionPostres . aplicarHechizo hechizo) postres

aplicarHechizo :: Hechizos -> [Postre] -> [Postre]
aplicarHechizo hechizo postres = map hechizo postres

validacionPostres :: [Postre] -> Bool
validacionPostres = all aplicarConsultas

aplicarConsultas :: Postre -> Bool
aplicarConsultas postre = (pesaAlgoMasQueCero postre) && (tieneSabor postre) && (noEstaCongelado postre)

pesaAlgoMasQueCero :: Postre -> Bool
pesaAlgoMasQueCero = (>0) . pesoPostre

tieneSabor :: Postre -> Bool
tieneSabor = (/=[]) . sabores

noEstaCongelado :: Postre -> Bool
noEstaCongelado = (>0) . temperatura

-- Ejercicio 1D --
--Dado un conjunto de postres en la mesa, conocer el peso promedio de los postres listos. 

pesoPromedioDePostresListos :: Hechizos -> [Postre] -> Number
pesoPromedioDePostresListos hechizo = (sumarPesosPostresFiltrados . filtrarPostres hechizo)

filtrarPostres :: Hechizos -> [Postre] -> [Postre]
filtrarPostres hechizo postres = filter (postresListos hechizo) postres

postresListos :: Hechizos -> Postre -> Bool
postresListos hechizo postre = estanPostresListo hechizo [postre]

sumarPesosPostresFiltrados :: [Postre] -> Number
sumarPesosPostresFiltrados = sum . pesosDePostres

pesosDePostres :: [Postre] -> [Number]
pesosDePostres = map pesoPostre

-- Ejercicio 2A--
--De un mago se conocen sus hechizos aprendidos y la cantidad de horrorcruxes que tiene. Hacer que un mago asista a la clase de defensa contra las cocinas oscuras y practique con un hechizo sobre un postre (se espera obtener el mago). Cuando un mago practica con un hechizo, lo agrega a sus hechizos aprendidos. Además si el resultado de usar el hechizo en el postre es el mismo que aplicarle “avada kedavra” al postre, entonces suma un horrorcrux.

data Mago = UnMago {
    hechizosAprendidos :: [Hechizos],
    cantidadHorrocruxes :: Number
} deriving (Show,Eq)

colorado :: Mago
colorado = UnMago {
    hechizosAprendidos = [(riddikulus "anana"), wingardiumLeviosa],
    cantidadHorrocruxes = 0
}

harryngui :: Mago
harryngui = UnMago{
    hechizosAprendidos = [incendio, (diffindo 25)],
    cantidadHorrocruxes = 1
}

hermione :: Mago
hermione = UnMago {
    hechizosAprendidos = [wingardiumLeviosa, immobulus, (diffindo 35)],
    cantidadHorrocruxes = 0
}

claseDeDefensaContraCocinasOsucras :: Hechizos -> Mago -> Postre -> Mago
claseDeDefensaContraCocinasOsucras hechizo mago postre = (validarHorrocruxes (agregarHechizoAMago hechizo mago) . aplicarHechizo hechizo) [postre]

agregarHechizoAMago :: Hechizos -> Mago -> Mago
agregarHechizoAMago hechizo mago = mago {hechizosAprendidos = (([hechizo]++) . hechizosAprendidos) mago}

validarHorrocruxes :: Mago -> [Postre] -> Mago
validarHorrocruxes mago (postre:resto) | condicionFinalPostre postre = mago {cantidadHorrocruxes = ((1+) . cantidadHorrocruxes) mago}
                                       | otherwise = mago

condicionFinalPostre :: Postre -> Bool
condicionFinalPostre postre = not ((noEstaCongelado postre) && (tieneSabor postre)) --No pude componer esto

-- Ejercicio 2B --
--Dado un postre y un mago obtener su mejor hechizo, que es aquel de sus hechizos que deja al postre con más cantidad de sabores luego de usarlo.

mejorHechizoDeMago :: Postre -> Mago -> Number 
mejorHechizoDeMago postre mago = foldl1 max (longitudesDeSabores postre mago)
--Logré que me devuelve la cantidad, no el hechizo. No tengo idea de como hacer que me devuelva el hechizo, ya que si hace eso no me da el nombre sino "<una funcion>"

longitudesDeSabores :: Postre -> Mago -> [Number]
longitudesDeSabores postre mago = (aplicarHechizoAUnMismoPostre postre . obtenerListaDeHechizos) mago

obtenerListaDeHechizos :: Mago -> [Hechizos]
obtenerListaDeHechizos mago = hechizosAprendidos mago 

aplicarHechizoAUnMismoPostre :: Postre -> [Hechizos] -> [Number]
aplicarHechizoAUnMismoPostre postre (hechizo:resto) = (length . (sabores . aplicarHechizoAUnPostre hechizo) $ postre) : aplicarHechizoAUnMismoPostre postre resto

aplicarHechizoAUnMismoPostre _ [] = []


aplicarHechizoAUnPostre :: Hechizos -> Postre -> Postre
aplicarHechizoAUnPostre hechizo postre = hechizo postre

-- Ejercicio 3B --
--Suponiendo que hay una mesa con infinitos postres, y pregunto si algún hechizo los deja listos ¿Existe alguna consulta que pueda hacer para que me sepa dar una respuesta? Justificar conceptualmente.

--No existe tal consulta, ya que al querer aplicar un hechizo al conjunto infinito de postres, el compilador nunca terminaría de aplicar los hechizos (tendríamos un loop infinito).

-- Ejercicio 3C --
--Suponiendo que un mago tiene infinitos hechizos ¿Existe algún caso en el que se puede encontrar al mejor hechizo? Justificar conceptualmente.

--SI, existe un caso, y sería aquel hechizo que más sobres agregue, sin embargo, nuevamente, el lenguaje estaría iterando infinitamente hasta encontrar aquél que tenga más sabores a agregar. 