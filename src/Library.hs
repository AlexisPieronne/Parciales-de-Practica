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

--Si, existe un caso, y sería aquel hechizo que más sobres agregue, sin embargo, nuevamente, el lenguaje estaría iterando infinitamente hasta encontrar aquél que tenga más sabores a agregar. 

------------------------------------------------------------------------------------------

-- Parcial UN PARCIAL MÁS --

-- Ejercicio 1 --
--Modelar los siguientes personajes principales: Mordecai tiene inteligencia 90, vagancia 60 y sus powerUps son videojuegos, taeKwonMortal con movimiento “Bloqueo” y picante. Posee los títulos “Extrahuevordinario” y “Golpe Mortal” ; Rigby tiene inteligencia 65, vagancia 101 y suspowerUps son cafeCafe de 200cm3, y taeKwonMortal con movimiento “Golpe”. No posee títulos.

type PowerUps = (Personaje -> Personaje)

data Personaje = UnPersonaje {
    nombrePersonaje :: String,
    nivelInteligencia :: Number,
    nivelVagancia :: Number,
    mejoras :: [PowerUps],
    titulos :: [String]
} deriving(Show,Eq)

mordecai :: Personaje
mordecai = UnPersonaje {
    nombrePersonaje = "Mordo",
    nivelInteligencia = 90,
    nivelVagancia = 60,
    mejoras = [videojuegos,taeKwonMortal "Bloqueo", picante],
    titulos = ["Extrahuevordinario","Golpe Mortal"]
}

rigby :: Personaje
rigby = UnPersonaje {
    nombrePersonaje = "Rigby",
    nivelInteligencia = 65,
    nivelVagancia = 101,
    mejoras = [cafeCafe 200, taeKwonMortal "Golpe", menteMax, fiestaLosMartes],
    titulos = []
}

papaleta :: Personaje
papaleta = UnPersonaje {nombrePersonaje = "Papaleta", nivelInteligencia = 0, nivelVagancia = 10, mejoras = [menteMax], titulos = ["Pops"]}

-- Ejercicio 2--
--Se pide crear los siguientes powerUps, para poder aplicárselos a un personaje: 1) fiestaLosMartes que reduce 10 puntos de vagancia y cambia el nombre del personaje al mismo nombre pero con bullicio al final. Ej: “Benson” cambia a “Benson Bullicio”. La vagancia nunca debe quedar por debajo de cero (ni con este ni con ningún otro powerup). 2) taeKwonMortal que dado el nombre de un movimiento, le agrega la palabra “mortal” y lo añade a los títulos del personaje. Tener en cuenta que un personaje no puede tener títulos repetidos (ni con este ni con ningún otro powerup). 3) menteMax agrega el título “Moriones”, y sube la inteligencia un 10%. 4) videoJuegos que agrega el título “Maestro de los videojuegos”, sube la inteligencia un 5% y aumenta 35 puntos de vagancia. 5) cafeCafe que reduce en 100 puntos la vagancia del personaje y aumenta la inteligencia 1% por cada 200cm3 de café ingeridos. 6) picante que no hace absolutamente nada, deja el personaje tal cual como esta.

videojuegos :: PowerUps
videojuegos = agregarTitulo "Maestro De Los Videojuegos" . modifInteligencia 5 . modifVagancia (+35)

modifVagancia :: (Number -> Number) -> Personaje -> Personaje
modifVagancia func personaje = personaje {nivelVagancia = max 0 ((func . nivelVagancia) personaje)}

modifInteligencia :: Number -> Personaje -> Personaje
modifInteligencia num personaje = personaje {nivelInteligencia = nivelInteligencia personaje + (nivelInteligencia personaje * num/100)}

agregarTitulo :: String -> Personaje -> Personaje
agregarTitulo titulo personaje | not (tieneElTitulo titulo personaje) = personaje {titulos = titulo : titulos personaje}
                               | otherwise = personaje

tieneElTitulo :: String -> Personaje -> Bool
tieneElTitulo titulo personaje = elem titulo (titulos personaje)

taeKwonMortal :: String -> PowerUps
taeKwonMortal str personaje = agregarTitulo (agregarMortal str) personaje  --No los pude componer debido a que agregarMortal no recibe un personaje

agregarMortal :: String -> String
agregarMortal str = str ++ " Mortal"

picante :: PowerUps
picante personaje = personaje

fiestaLosMartes :: PowerUps
fiestaLosMartes personaje = (modifNombre (++" Bullicio") . modifVagancia (subtract 10)) personaje

modifNombre :: (String -> String) -> Personaje -> Personaje
modifNombre func personaje = personaje {nombrePersonaje = (func . nombrePersonaje) personaje}

menteMax :: PowerUps
menteMax personaje = (agregarTitulo "Moriones" . modifInteligencia 10) personaje

cafeCafe :: Number -> PowerUps
cafeCafe valor personaje = (modifInteligencia (valor/200). modifVagancia (subtract 100)) personaje

-- Ejercicio 3--
--Dado un personaje y una misión, nos interesa saber si ese personaje puede realizar esa misión teniendo en cuenta que para cualquier misión es requisito que el personaje tenga al menos 1 powerUp. Además, se pide modelar las siguientes misiones: 1) desafioExtrahuevordinario que se puede realizar únicamente si el personaje tiene el título “Extrahuevordinario”. 2) darCarinio que se puede realizar siempre salvo que el personaje sea “Rigby”. 3) beberMissisipiQueen que sólo la pueden realizar quienes se llamen Mordecai, Benson o Rigby y no sean vagos. Alguien es vago cuando tiene 70 puntos de vagancia o más. 4) comerSandwichDeLaMuerte que se puede realizar si el personaje tiene algún movimiento mortal (título que termina con la palabra “mortal”) (recordar el requisito común de tener al menos 1 powerUp).

type Mision = Personaje -> Bool

puedeHacerMision :: Mision -> Personaje -> Bool
puedeHacerMision mision personaje = (tieneAlMenos1PowerUp personaje) && (cumpleRequisitoDeMision personaje mision)

tieneAlMenos1PowerUp :: Personaje -> Bool
tieneAlMenos1PowerUp personaje = not ((null . mejoras) personaje)

cumpleRequisitoDeMision :: Personaje -> Mision -> Bool
cumpleRequisitoDeMision personaje mision = mision personaje  --Esta función se me hace super redundante

desafioExtrahuevordinario :: Mision
desafioExtrahuevordinario personaje = elem "Extrahuevordinario" (titulos personaje)

darCarinio :: Mision
darCarinio personaje = nombrePersonaje personaje /= "Rigby"

beberMissisipiQueen :: Mision
beberMissisipiQueen personaje = (validarNombre personaje) && (not (esVago personaje))

validarNombre :: Personaje -> Bool
validarNombre personaje = (nombrePersonaje personaje == "Mordo") || (nombrePersonaje personaje == "Rigby") || (nombrePersonaje personaje == "Benson")

esVago :: Personaje -> Bool
esVago personaje = ((>70) . nivelVagancia) personaje

comerSandwichDeLaMuerte :: Mision
comerSandwichDeLaMuerte personaje = elem "Mortal" (titulos personaje) --No tengo idea de como hacer para que busque un título que termine en "Mortal", lo que puse ahí solo busca por el título "Mortal"

-- Ejercici 4 -- 
--Escribir los tests necesarios para probar que funcione bien la misión missisipiQueen.

-- Ejercicio 5 --
--Dada una lista de personajes y una misión deseamos saber si es un grupo regular para esa misión. Es un grupo regular cuando más de 3 integrantes pueden realizar la misión o si hay uno llamado ”Papaleta” entre ellos. Ni siquiera es necesario que Papaleta pueda realizarla, con estar ahí aporta suficiente.

esGrupoRegular :: [Personaje] -> Mision -> Bool
esGrupoRegular grupo mision = masDe3PuedenHacerla grupo mision || hayUnPapaleta grupo

hayUnPapaleta :: [Personaje] -> Bool
hayUnPapaleta grupo = ((elem "Papaleta") . filtrarNombres) grupo

filtrarNombres :: [Personaje] -> [String]
filtrarNombres grupo = map nombrePersonaje grupo

masDe3PuedenHacerla :: [Personaje] -> Mision -> Bool
masDe3PuedenHacerla grupo mision = ((>3) . length . (puedenHacerla grupo)) mision

puedenHacerla :: [Personaje] -> Mision -> [Bool]
puedenHacerla grupo mision = (filtrarLosQuePueden . evaluarLaMisionConTodos mision) grupo

evaluarLaMisionConTodos :: Mision -> [Personaje] -> [Bool]
evaluarLaMisionConTodos mision grupo = map mision grupo

filtrarLosQuePueden :: [Bool] -> [Bool]
filtrarLosQuePueden grupo = filter id grupo --No conocía la función id, la encontré en un foro. Es posible usarla?

-- Ejercicio 6 --
--Se desea conocer la versión suprema de un personaje. Este es el personaje luego de haberse aplicado sus Power Ups sobre sí mismo

versionSuprema :: Personaje -> Personaje
versionSuprema personaje = (aplicarPowerUps personaje . obtenerPowerUps) personaje

obtenerPowerUps :: Personaje -> [PowerUps]
obtenerPowerUps personaje = mejoras personaje

aplicarPowerUps :: Personaje -> [PowerUps] -> Personaje
aplicarPowerUps personaje mejoras = foldl aplica1SoloPowerUp personaje mejoras

aplica1SoloPowerUp :: Personaje -> PowerUps -> Personaje
aplica1SoloPowerUp personaje mejora = mejora personaje

-- Ejercicio 7 --
--Dada una lista de misiones y un personaje: a) Se desea conocer cuántas misiones seguidas puede cumplir ese personaje estando en su versión suprema. Tener en cuenta que deja de contar cuando se cruza con una que no pueda completar. b) ¿Qué pasaría si consulto el punto anterior con una lista infinita de misiones?I) Analizar conceptualmente II) codificar al menos una lista infinita de ejemplo.

cuantasPuedeHacer :: [Mision] -> Personaje -> Number
cuantasPuedeHacer [] _ = 0
cuantasPuedeHacer (x:xs) personaje | (puedeHacerMision x . versionSuprema) personaje = 1 + cuantasPuedeHacer xs personaje
                                   | otherwise = 0

-- b) Debido al enunciado del problema, Haskell iteraría hasta encontrar una mision que no pueda completarse por el personaje. Al encontrarla devolvería la cantidad de misiones que fueron completadas. En caso de que todas las misiones pudieran completasrse, el lenguaje estaría iterando infinitamente.

listaInfinitaDeMisiones :: [PowerUps]
listaInfinitaDeMisiones = repeat picante

------------------------------------------------------------------------------------------

-- Parcial HASKELLPARK --

-- Ejercicio 1a --
--Modelamos una aplicación de parques de diversiones donde registramos sus atracciones, de las cuales conocemos su nombre, altura mínima requerida para la persona que ingrese medida en centímetros, duración en minutos, una serie de opiniones que le da la gente (“entretenida”, “veloz”, ”un embole”, etc) y si está en mantenimiento o no lo cual permite el acceso por parte de las personas. Un operador determina si el entretenimiento requiere atención y lo pasa a su estado de mantenimiento. En algún momento pasan los técnicos y asignan las reparaciones, que tiene una duración determinada en días y el trabajo que se realiza.

data Atracciones = UnaAtraccion {
    nombreAtraccion :: String,
    alturaMinimaEnCM :: Number,
    duracionEnMinutos :: Number,
    opiniones :: [String],
    reparaciones :: Mantenimiento
} deriving(Show,Eq)

data Mantenimiento = UnMantenimiento {
    estaEnMantenimiento :: Bool,
    ordenesDeReparacion :: Number,
    reparacionesARealizar :: [Reparacion],
    duracionDeReparaciones :: [Number]
} deriving(Show,Eq)

puente :: Atracciones
puente = UnaAtraccion {
    nombreAtraccion = "Puentesuli",
    alturaMinimaEnCM = 160,
    duracionEnMinutos = 7,
    opiniones = ["Estresante","Divertida","Un Embole"],
    reparaciones =  (UnMantenimiento True 2 [engrase 5, ajusteDeTornillería 1] [2,5])
}

saltoMortal:: Atracciones
saltoMortal = UnaAtraccion {
    nombreAtraccion = "El Salto Mortal",
    alturaMinimaEnCM = 170,
    duracionEnMinutos = 5,
    opiniones = ["Alocada","Emocionante","Extrema"],
    reparaciones =  (UnMantenimiento False 0 [] [])
}

type Reparacion = Atracciones -> Atracciones

-- Ejercicio 1b --
--Queremos saber qué tan buena es una atracción. Para eso utilizamos un sistema de scoring que tiene un modo muy particular para calcularlo: Si la atracción tiene una duración prolongada (es decir que dura más de 10 minutos) valen 100 puntos. Si no es así, pero tiene menos de 3 órdenes de reparaciones el puntaje es 10 puntos por cada letra del nombre más 2 puntos por cada opinión que tiene. Caso contrario es 10 veces la altura mínima requerida.

queTanBuenaEs :: Atracciones -> Number
queTanBuenaEs atraccion | duracionMayorA atraccion = 100
                        | ordenesReparacionMenorA atraccion = puntosDeAtraccion atraccion
                        | otherwise = puntosPorAltura atraccion

duracionMayorA :: Atracciones -> Bool
duracionMayorA atraccion = ((>10) . duracionEnMinutos) atraccion

ordenesReparacionMenorA :: Atracciones -> Bool
ordenesReparacionMenorA atraccion = verMantenimientos (reparaciones atraccion)

verMantenimientos :: Mantenimiento -> Bool
verMantenimientos reparacion = ((<3) . ordenesDeReparacion) reparacion

puntosPorAltura :: Atracciones -> Number
puntosPorAltura atraccion = ((10*). alturaMinimaEnCM) atraccion

puntosDeAtraccion :: Atracciones -> Number
puntosDeAtraccion atraccion = ((+puntosPorNombre atraccion) . puntosPorOpinion) atraccion

puntosPorNombre :: Atracciones -> Number
puntosPorNombre atraccion = ((10*) . length . nombreAtraccion) atraccion

puntosPorOpinion :: Atracciones -> Number
puntosPorOpinion atraccion = ((2*) . length . opiniones) atraccion

--Siento que estas últimas 2 funciones las podía unir en una sola parando una función. Siento que de esta manera repiten lógica 

-- Ejercicio 2 --
--Los técnicos tienen diversos tipos de trabajo que pueden desarrollar en cada reparación sobre las atracciones. Algo muy importante a tener en cuenta es que luego de que realizan cualquier trabajo siempre ocurren dos cosas: se elimina la última reparación de la lista (no importa cual fuere) y se verifica que no tenga reparaciones pendientes. Si quedan pendientes debe mantener el indicador que está en mantenimiento, caso contrario no. Los posibles trabajos son : 1) ajusteDeTornillería que como le refuerza aún más estructura a la atracción, prolonga su duración en 1 minuto por cada tornillo apretado pero no pudiendo superar los 10 minutos porque no es rentable. Es decir que si una atracción dura 3 minutos y ajusta 4 tornillos la misma pasa a durar 7 minutos. Pero sí una atracción dura 8 minutos y el técnico logra apretar 5 tornillos pasa a durar solamente 10 minutos. 2) engrase que vuelve más veloz al entretenimiento, por lo tanto aumenta en 0,1 centímetros la altura mínima requerida por cada gramo de grasa utilizada en el proceso y le agrega la opinión “para valientes”. La cantidad de grasa requerida puede variar según el criterio del técnico. 3) mantenimientoElectrico repara todas las bombitas de luz y su cableado. Como es un lavado de cara y una novedad para la gente, solo se queda con las dos primeras opiniones y el resto las descarta. 4) mantenimientoBásico que consiste en ajustar la tornillería de 8 tornillos y hacer un engrase con 10 gramos de grasa. 

ajusteDeTornillería :: Number -> Reparacion
ajusteDeTornillería cantTornillos atraccion = atraccion {duracionEnMinutos = ((min 10).(duracionEnMinutos . modifDuracion cantTornillos)) atraccion, reparaciones = restaurarAtraccion atraccion}

restaurarAtraccion :: Atracciones -> Mantenimiento
restaurarAtraccion atraccion = UnMantenimiento False (restar1Trabajo atraccion) (eliminarUltimoTrabajo atraccion) (eliminarUltimaDuracion atraccion)

restar1Trabajo :: Atracciones -> Number
restar1Trabajo atraccion = subtract 1 (ordenesDeReparacion (reparaciones atraccion))

eliminarUltimoTrabajo :: Atracciones -> [Reparacion]
eliminarUltimoTrabajo atraccion = init (reparacionesARealizar (reparaciones atraccion))

eliminarUltimaDuracion :: Atracciones -> [Number]
eliminarUltimaDuracion atraccion = init (duracionDeReparaciones (reparaciones atraccion))

modifDuracion :: Number -> Atracciones -> Atracciones
modifDuracion cantTornillos atraccion = atraccion {duracionEnMinutos = ((+cantTornillos). duracionEnMinutos) atraccion}

engrase :: Number -> Reparacion
engrase gramosGrasa atraccion = atraccion {alturaMinimaEnCM = modifAltura gramosGrasa atraccion, opiniones = agregarOpinion "Para Valientes" atraccion, reparaciones = restaurarAtraccion atraccion}

modifAltura :: Number -> Atracciones -> Number
modifAltura cant atraccion = ((+(0.1*cant)) . alturaMinimaEnCM) atraccion

agregarOpinion :: String -> Atracciones -> [String]
agregarOpinion opinion atraccion = ((opinion:) . opiniones) atraccion

mantenimientoElectrico :: Reparacion
mantenimientoElectrico atraccion = atraccion {opiniones = lasDosPrimeras atraccion, reparaciones = restaurarAtraccion atraccion}

lasDosPrimeras :: Atracciones -> [String]
lasDosPrimeras atraccion = (take 2 . opiniones) atraccion

mantenimientoBasico :: Reparacion
mantenimientoBasico atraccion = (ajusteDeTornillería 8 . engrase 10) atraccion

-- Ejercicio 3a --
--Esa me da miedito: Queremos saber si una atracción meDaMiedito, esto implica que alguna de las inspecciones que se le hicieron le asignó más de 4 días de mantenimiento.

meDaMiedito :: Atracciones -> Bool
meDaMiedito atraccion = tieneDuracionMayorA4 (reparaciones atraccion)

tieneDuracionMayorA4 :: Mantenimiento -> Bool
tieneDuracionMayorA4 reparacion = (any (>4) . duracionDeReparaciones) reparacion

-- Ejercicio 3b --
--Acá cerramos: Cerramos una atracción si la sumatoria de tiempo de las reparaciones pendientes para dicha atracción es de 7 días. 

acaCerramos :: Atracciones -> Bool
acaCerramos atraccion = sumanMasDe7Dias (reparaciones atraccion)

sumanMasDe7Dias :: Mantenimiento -> Bool
sumanMasDe7Dias reparacion = ((>=7) . sum . duracionDeReparaciones) reparacion

-- Ejercicio 3c --
--Disney no esistis: Tenemos que determinar disneyNoEsistis para un parque. Esto ocurre cuando todas las atracciones de nombre cheto (con más de 5 letras) no tienen reparaciones pendientes.

disneyNoEsistis :: [Atracciones] -> Bool
disneyNoEsistis atracciones = (all ((==0) . ordenesDeReparacion . reparaciones) . filter ((>5) . length . nombreAtraccion)) atracciones

--nombreCheto :: [Atracciones] -> [Atracciones]
--nombreCheto atracciones = filter ((>5) . length . nombreAtraccion) atracciones

--noTienenReparaciones :: [Atracciones] -> Bool
--noTienenReparaciones atracciones = all ((==0) . ordenesDeReparacion . reparaciones) atracciones

-- Ejercicio 4 --
--Una atracción tiene reparaciones peolas si luego de cada una está más buena, esto implica que luego de hacer el trabajo de cada reparación el puntaje mejora con respecto a la reparación previa. 

tieneReparacionesPeolas :: Atracciones -> Bool
tieneReparacionesPeolas atraccion = ((aplicarReparacionesYverificar atraccion) . obtenerReparaciones) atraccion

obtenerReparaciones :: Atracciones -> [Reparacion]
obtenerReparaciones atraccion = (reparacionesARealizar . reparaciones) atraccion

aplicarReparacionesYverificar :: Atracciones -> [Reparacion] -> Bool
aplicarReparacionesYverificar atraccion (reparacion:resto) | (queTanBuenaEs (atraccionReparada atraccion reparacion)) > (queTanBuenaEs atraccion) = True && aplicarReparacionesYverificar atraccion resto
                                                           | otherwise = False
aplicarReparacionesYverificar atraccion [] = True

atraccionReparada :: Atracciones -> Reparacion -> Atracciones
atraccionReparada atraccion reparacion = reparacion atraccion

-- Ejercicio 5 --
--Queremos modelar un proceso que realice los trabajos de las reparaciones pendientes sobre una atracción. Se pide que además muestre un ejemplo de cómo podría evaluar por consola el proceso para cada una de las actividades resueltas en el punto anterior.

repararAlCompleto :: Atracciones -> Atracciones
repararAlCompleto atraccion = (aplicarReparaciones atraccion . obtenerReparaciones) atraccion 

aplicarReparaciones :: Atracciones -> [Reparacion] -> Atracciones
aplicarReparaciones atraccion reparaciones = foldl aplicar1SolaReparacion atraccion reparaciones 

aplicar1SolaReparacion :: Atracciones -> Reparacion -> Atracciones
aplicar1SolaReparacion atraccion reparacion = reparacion atraccion 

-- No entendí lo que pide que hagamos por consola

-- Ejercicio 6 --
--Si una atracción tiene una cantidad infinita de trabajos, ¿sería posible obtener un valor computable para la función del punto anterior? ¿Qué ocurriría con una lista de trabajos infinita en el punto 4? Justifique sus respuestas relacionándolo con un concepto visto en la materia.

--Para el punto anterior sería imposible que el lenguaje nos entregue un valor computable, ya que estaría evaluando y aplicando reparaciones infinitamente. Esto debido a que la "condicion de corte" es la cantidad de reparaciones a realizar. En el punto 4, en cambio, el lenguaje va a iterar hasta encontrar una que no cumpla la condición establecida (Que al mejorarle este mas buena que su versión anterior). 