module MicroEntrega1 where
import Text.Show.Functions

--3.1 Pto1.1

data Microprocesador = Microprocesador {
	memoria :: [Int],
	acumuladorA :: Int,
	acumuladorB :: Int,
	programCounter :: Int,
	mensajeError :: String 
} deriving (Show)



--3.1 Pto1.2

{-xt8088 :: Microprocesador
xt8088 = Microprocesador {memoria = [], acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = ""}-}

xt8088 :: Microprocesador
xt8088 = Microprocesador {memoria = replicate 1024 0, acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = ""}

fp20 :: Microprocesador
fp20 = Microprocesador {memoria = [], acumuladorA = 7, acumuladorB = 24, programCounter = 0, mensajeError = ""}

at8086 :: Microprocesador
at8086 = Microprocesador {memoria = [1..20], acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = ""}
	
--3.2 Pto2.1

nop :: Microprocesador -> Microprocesador
nop microprocesador = microprocesador {programCounter = incrementoPC programCounter microprocesador}

--3.3 Pto3.1

lodv :: Int -> Microprocesador -> Microprocesador
lodv val microprocesador = microprocesador { acumuladorA = val , programCounter = incrementoPC programCounter microprocesador }

swap :: Microprocesador -> Microprocesador
swap microprocesador = microprocesador { acumuladorA = acumuladorB microprocesador, acumuladorB= acumuladorA microprocesador , programCounter = incrementoPC programCounter microprocesador}

add :: Microprocesador -> Microprocesador
add microprocesador = microprocesador  { acumuladorA = (acumuladorA microprocesador) + (acumuladorB microprocesador),  acumuladorB = 0 , programCounter = incrementoPC programCounter microprocesador}

--3.4 Pto4.1

divide :: Microprocesador -> Microprocesador
divide microprocesador
       |acumuladorB microprocesador /= 0 = microprocesador {acumuladorA = div (acumuladorA microprocesador) (acumuladorB microprocesador), acumuladorB = 0, programCounter = incrementoPC programCounter microprocesador, mensajeError = "" }
       |otherwise  = microprocesador {acumuladorA = 0, programCounter = incrementoPC programCounter microprocesador, mensajeError = "DIVISION ES ZERO" } 
  
str :: Int -> Int -> Microprocesador -> Microprocesador
str addr val microprocesador = microprocesador { memoria = (take (addr-1) (memoria microprocesador) ++ [val] ++ (drop addr (memoria microprocesador))) , programCounter = incrementoPC programCounter microprocesador}

lod :: Int -> Microprocesador -> Microprocesador
lod addr microprocesador = microprocesador { acumuladorA= ((!!) (memoria microprocesador) (addr-1)) ,  programCounter = incrementoPC programCounter microprocesador}

incrementoPC pc constructor = pc constructor +1

--------------- CASOS DE PRUEBA ---------------

-- para podes avanzar 3 posiciones el program counter es necesario utilizar COMPOSICION de la funcion NOP (nop.nop.nop)
-- avanzaTresPosiciones = (nop.nop.nop)

-- carga valor 5 en acumulador A
-- cincoEnAcumuladorA = lodv 5

--Intercambiar valores de acumuladores (de microprocesador fp20)
-- IntercambioDeValoresDeAcumuladores = swap 

-- programa que permite sumar 10 + 22
--diezMasVentidos = (add.(lodv 22).swap.(lodv 10)) 

--Aplica STR 2 5 (en microprocesador AT8086)
-- cincoEnPosicionDos = str 2 5

--Aplica LOD 2 ( en xt8088)
-- posicionDosEnAcumuladorA = lod 2

--   dividir 2 por 0
-- 	(divide.(lod 1).swap.(lod 2).(str 2 0).(str 1 2))

-- dividir 12 por 4
--  (divide.(lod 1).swap.(lod 2).(str 2 4).(str 1 12))