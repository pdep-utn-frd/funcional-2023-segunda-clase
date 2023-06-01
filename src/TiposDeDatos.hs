{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module TiposDeDatos where


--  que es una tupla?
-- un par ordenado

data EstadoDeVacunacion = Estado String Bool deriving (Show)

data Complejo = UnComplejo Float Float deriving (Show)

conjugado :: Complejo -> Complejo
conjugado (UnComplejo real imaginaria) = UnComplejo real (-imaginaria) 

esCero :: Complejo -> Bool
esCero (UnComplejo 0 0) = True
esCero _ = False

data Alumno = UnAlumno String Integer Char deriving (Show)

nombreAlumno :: Alumno -> String
nombreAlumno (UnAlumno nombre _ _) = nombre

especialidadAlumno :: Alumno -> Char
especialidadAlumno (UnAlumno _ _ esp) = esp

legajo ::  Alumno -> Integer
legajo (UnAlumno nombre legajo especialidad) = legajo


-- records
data RecordAlumno = UnAlumnoR { nombre :: String, 
                                legajoAlumno :: Integer,
                                especialidad :: Char } deriving (Show, Eq, Ord)


data Tupla4Elementos = T4 { fst_t4 :: Int, snd_t4 :: Int, thrd :: Int, fourth :: Int} deriving Show



-- Tipos con variantes

data Dolar = DolarOficial Float 
            | DolarBlue Float
            | DolarTurista Float
            | DolarQatar Float 
            deriving Show

data PesoArgentino = Peso Float deriving Show

pasarAPesos :: Dolar -> PesoArgentino
pasarAPesos (DolarOficial cantidad) = Peso (cantidad * 220)
pasarAPesos (DolarBlue c) = Peso (c * 460)
pasarAPesos (DolarQatar c) = undefined
pasarAPesos (DolarTurista c) = undefined



data Booleano = Verdadero | Falso deriving Show

conjugacion :: Booleano -> Booleano -> Booleano
conjugacion Verdadero Verdadero = Verdadero
conjugacion _ _ = Falso

cambiarseDeEspecialidad :: RecordAlumno -> Char -> RecordAlumno
cambiarseDeEspecialidad (UnAlumnoR nombre legajoAlumno _) nuevaEspecialidad
    = UnAlumnoR nombre legajoAlumno nuevaEspecialidad


data Claustros = Docente {nombreDocente :: String} 
               | NoDocente {nombreNoDocente :: String}
               | Graduado {nombreGraduado :: String, añoGraduacion :: Int}
               | Alumno { datosAlumno :: RecordAlumno }
               deriving (Show)

cantidadLetrasNombre :: Claustros -> Int
cantidadLetrasNombre (Docente nombre) = length nombre
cantidadLetrasNombre (NoDocente nombre) = length nombre
cantidadLetrasNombre (Graduado nombre año) = length nombre + año
cantidadLetrasNombre a = length (nombre (datosAlumno a))

-- Tipos recursivos

data Natural = Cero | Siguiente Natural deriving (Show)

suma :: Natural -> Natural -> Natural
suma Cero n = n
suma n Cero = n
suma (Siguiente n) (Siguiente m) = suma (Siguiente (Siguiente n)) m

data Lista = ListaVacia | UnaLista Int Lista
                                -- cabeza
                                --      cola

cabezaDeLista :: Lista -> Int
cabezaDeLista ListaVacia = error "las listas vacias no tienen cabeza"
cabezaDeLista (UnaLista elemento _) = elemento

cabezaDeUnaListaPosta :: [Int] -> Int
cabezaDeUnaListaPosta [] = error "las listas vacias no tienen cabeza"
cabezaDeUnaListaPosta (elemento : _cola) = elemento 

largoDeUnaLista :: [Int] -> Int
largoDeUnaLista [] = 0
largoDeUnaLista (_cabeza:cola) = 1 + largoDeUnaLista cola

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x : xs) = x + sumatoria xs

concatenar :: [String] -> String
concatenar [] = ""
concatenar (unString : muchosStrings) = unString ++ concatenar muchosStrings

                                        -- rama derecha
                                        --            rama izq.
data ArbolBinario = Hoja Int | Rama Int ArbolBinario ArbolBinario deriving Show 

sumatoriaArbol :: ArbolBinario -> Int
sumatoriaArbol (Hoja 0) = 12
sumatoriaArbol (Hoja 12) = 0
sumatoriaArbol (Hoja x) = 1 
sumatoriaArbol (Rama valor ramaDer ramaIzq) 
    = valor + sumatoriaArbol ramaDer + sumatoriaArbol ramaIzq

cantidadDeHojas :: ArbolBinario -> Int
cantidadDeHojas (Hoja _) = 1
cantidadDeHojas (Rama _ ramaDer ramaIzq)
    = cantidadDeHojas ramaDer + cantidadDeHojas ramaIzq

x = length [1 .. 100000000]


largoTailRecursive lista = largoAux lista 0

largoAux [] acum = acum
largoAux (x:xs) acum = largoAux xs (acum + 1)

