module MicroEntrega2 where
import Text.Show.Functions

--3.1 Pto1.1

-- MODIFICACIONES AL DATA PARA INCLUIR PROGRAMA

type Instrucciones = Microprocesador -> Microprocesador -- parte 2

data Microprocesador = Microprocesador {
	memoria :: [Int],
	instrucciones :: [Instrucciones], -- parte 2 -- ISSUE
	acumuladorA :: Int,
	acumuladorB :: Int,
	programCounter :: Int,
	mensajeError :: String 
} deriving (Show)

incrementoPC :: Microprocesador -> Microprocesador
incrementoPC microprocesador = microprocesador {programCounter = programCounter microprocesador +1}


--3.1 Pto1.2

{-xt8088 :: Microprocesador
xt8088 = Microprocesador {memoria = [], instrucciones = [], acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = ""}-}

xt8088 :: Microprocesador
xt8088 = Microprocesador {memoria = replicate 1024 0, instrucciones = [ ], acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = ""}

fp20 :: Microprocesador
fp20 = Microprocesador {memoria = [], instrucciones = [], acumuladorA = 7, acumuladorB = 24, programCounter = 0, mensajeError = ""}

at8086 :: Microprocesador
at8086 = Microprocesador {memoria = [1..20], instrucciones = [], acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = ""}


-- MODELADOS PARTE 2

microprocesadorMemoriaInf = Microprocesador {memoria = [0,0 ..], instrucciones = [], acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = ""}

microDesorden =  Microprocesador {memoria = [2,5,1,0,6,9], instrucciones = [], acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = ""} 

--3.2 Pto2.1

nop :: Microprocesador -> Microprocesador
nop = incrementoPC

--3.3 Pto3.1

lodv :: Int -> Microprocesador -> Microprocesador
lodv val = incrementoPC.cargarAcumuladorA val

cargarAcumuladorA :: Int -> Microprocesador -> Microprocesador
cargarAcumuladorA val microprocesador = microprocesador { acumuladorA = val }

swap :: Microprocesador -> Microprocesador
swap = incrementoPC.intercambioAcumuladores

intercambioAcumuladores :: Microprocesador -> Microprocesador
intercambioAcumuladores microprocesador = microprocesador { acumuladorA = acumuladorB microprocesador, acumuladorB= acumuladorA microprocesador}

add :: Microprocesador -> Microprocesador
add = incrementoPC.sumaAcumuladoresEnA

ceroEnB :: Microprocesador -> Microprocesador
ceroEnB microprocesador = microprocesador { acumuladorB = 0}

sumaAcumuladoresEnA :: Microprocesador -> Microprocesador
sumaAcumuladoresEnA microprocesador = ceroEnB.cargarAcumuladorA (acumuladorA microprocesador + acumuladorB microprocesador) $ microprocesador --ISSUE

--3.4 Pto4.1

divide :: Microprocesador -> Microprocesador
divide = incrementoPC.divideAporB

divideAporB :: Microprocesador -> Microprocesador
divideAporB microprocesador
       |acumuladorB microprocesador /= 0 = microprocesador {acumuladorA = div (acumuladorA microprocesador) (acumuladorB microprocesador), acumuladorB = 0, mensajeError = "" }
       |otherwise  = microprocesador {acumuladorA = 0, mensajeError = "DIVISION ES ZERO" } 
  
str :: Int -> Int -> Microprocesador -> Microprocesador
str addr val = incrementoPC.guardarValorEnMemoria addr val 

guardarValorEnMemoria ::  Int -> Int -> Microprocesador -> Microprocesador
guardarValorEnMemoria addr val microprocesador = microprocesador { memoria = (take (addr-1) (memoria microprocesador) ++ [val] ++ (drop addr (memoria microprocesador))) }

lod :: Int -> Microprocesador -> Microprocesador
lod addr = incrementoPC.cargarMemoriaEnAcumuladorA addr

cargarMemoriaEnAcumuladorA :: Int -> Microprocesador -> Microprocesador
cargarMemoriaEnAcumuladorA addr microprocesador = microprocesador { acumuladorA= ((memoria microprocesador) !! (addr-1))} --ISSUE


-- programa que permite sumar 10 + 22  (PARTE2)

diezMasVeintidos :: [Instrucciones]
diezMasVeintidos = [add,lodv 22,swap,lodv 10]

--   dividir 2 por 0 (PARTE2)

divisionDosPorCero :: [Instrucciones]
divisionDosPorCero = [divide,lod 1,swap,lod 2,str 2 0,str 1 2]

-- Cargar programa al microprocesador -- parte2

cargarProgramaAlMicro :: [Instrucciones] -> Instrucciones 
cargarProgramaAlMicro programaNuevo microprocesador = microprocesador {instrucciones = programaNuevo}

-- Ejecutar programa del microprocesador -- parte 2 (con lista para poder frenar cuando hay error)
ejecutarPrograma :: Instrucciones
ejecutarPrograma microprocesador = foldr ejecutarHastaError microprocesador (instrucciones microprocesador)

ejecutarHastaError :: Instrucciones -> Instrucciones
ejecutarHastaError instruccion microprocesador
	| noHayError microprocesador = instruccion microprocesador
	|otherwise = microprocesador

noHayError :: Microprocesador -> Bool
noHayError microprocesador = null (mensajeError microprocesador) --ISSUE 

-- instruccion IFNZ (PARTE2)
ifnz :: [Instrucciones] -> Microprocesador -> Microprocesador
ifnz instrucciones microprocesador
   | acumuladorA microprocesador /= 0 = ejecutarPrograma.cargarProgramaAlMicro  instrucciones $ microprocesador
   | otherwise = microprocesador

-- Depurar Programa (PARTE2)
depurar :: [Instrucciones] -> Microprocesador -> [Instrucciones] -- filtra la lista de instrucciones y deja solo las utiles
depurar instrucciones microprocesador = filter (instruccionUtil microprocesador) instrucciones

instruccionUtil :: Microprocesador -> Instrucciones -> Bool -- ejecuta una instruccion, convierte en lista lo que nos importa y ve si los valores son distintos de cero
instruccionUtil microprocesador instruccion = any (/= 0)(valoresAEvaluar.instruccion $ microprocesador)

valoresAEvaluar :: Microprocesador -> [Int] -- lista con acumulado A,B y memoria 
valoresAEvaluar microprocesador = [acumuladorA microprocesador, acumuladorB microprocesador] ++ (memoria microprocesador)

-- Memoria Ordenada (PARTE2)

listaOrdenada :: [Int] -> Bool
listaOrdenada [] = True
listaOrdenada [_]= True --ISSUE
listaOrdenada (x:y:ys) = x <= y && listaOrdenada (y:ys) -- ISSUE


memoriaOrdenada :: Microprocesador -> Bool   
memoriaOrdenada microprocesador = listaOrdenada (memoria microprocesador)

-- Memoria Infinita (PARTE2)

-- cargar y ejecutar programa en microprocesador de memoria infinita

programaEnMemInf :: Microprocesador
programaEnMemInf = ejecutarPrograma.cargarProgramaAlMicro  diezMasVeintidos $ microprocesadorMemoriaInf

-- es posible cargar y ejecutar un programa en un microprocesador con memoria infinita, pero
-- al ejecutarse no terminara de mostrar la memoria.


-- memoria ordenada en microprocesador de memoria infinita

ordenarMicroMemInf :: Bool
ordenarMicroMemInf = memoriaOrdenada microprocesadorMemoriaInf

-- La consola se tilda al querer ver si la memoria esta ordenada ya que nunca termina de leerla


--------------- CASOS DE PRUEBA Parte 1---------------

-- para podes avanzar 3 posiciones el program counter es necesario utilizar COMPOSICION de la funcion NOP (nop.nop.nop)
avanzaTresPosiciones = (nop.nop.nop)

-- carga valor 5 en acumulador A
cincoEnAcumuladorA = lodv 5

--Intercambiar valores de acumuladores (de microprocesador fp20)
intercambioDeValoresDeAcumuladores = swap 

-- programa que permite sumar 10 + 22
diezMasVientidos = (add.(lodv 22).swap.(lodv 10)) 

--Aplica STR 2 5 (en microprocesador AT8086)
cincoEnPosicionDos = str 2 5

--Aplica LOD 2 ( en xt8088)
posicionDosEnAcumuladorA = lod 2

--   dividir 2 por 0
dividirDosyCero = (divide.(lod 1).swap.(lod 2).(str 2 0).(str 1 2))

-- dividir 12 por 4
dividirDoceyCuatro = (divide.(lod 1).swap.(lod 2).(str 2 4).(str 1 12))

-----------CASOS DE PRUEBA Parte 2 -----------

-- Pruebas de Programas
cargarDiezMasVeintidos = cargarProgramaAlMicro diezMasVeintidos xt8088
cargarDivisionDosPorCero = cargarProgramaAlMicro divisionDosPorCero xt8088

ejecutarDiezMasVeintidos = ejecutarPrograma cargarDiezMasVeintidos 
ejecutarDivisionDosPorCero = ejecutarPrograma cargarDivisionDosPorCero 

-- Pruebas sobre IFNZ
funcionesParaIFNZ = [swap, lodv 3]
ejecutarIFNZ1 = ifnz funcionesParaIFNZ fp20
ejecutarIFNZ2 = ifnz funcionesParaIFNZ xt8088

-- Depuracion del programa
funcionesParaDepurar = [lodv 133, str 1 3]
ejecutarDepuracion = depurar funcionesParaDepurar xt8088

-- Orden de la memoria
ejecutarMemoriaOrdenada1 = memoriaOrdenada at8086
ejecutarMemoriaOrdenada2 = memoriaOrdenada microDesorden
