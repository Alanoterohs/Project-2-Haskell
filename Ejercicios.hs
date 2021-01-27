--Ejercicio 1

data Carrera = Matematica | Fisica | Computacion | Astronomia

titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matematicas"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en Computacion"
titulo Astronomia = "Licenciatura en Astronomia"

--Ejercicio 2

data Carrera = Matematica | Fisica | Computacion | Astronomia deriving (Show, Eq, Ord, Bounded)

$> Matematica <= Computacion
True

$> Matematica ‘min‘ Computacion
Matematica

--Ejercicio 3

type Ingreso = Int

data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar
data Area = Administrativa | Ensenanza | Economica | Postgrado

data Persona = Decano | Docente Cargo | NoDocente Area | Estudiante Carrera Ingreso

--b) el tipo del constructor Docente es Docente :: Cargo -> Persona

--c)
data Carrera = Matematica | Fisica | Computacion | Astronomia deriving (Eq)
type Ingreso = Int
data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar deriving (Eq)
data Area = Administrativa | Ensenanza | Economica | Postgrado deriving (Eq)
data Persona = Decano | Docente Cargo | NoDocente Area | Estudiante Carrera Ingreso deriving (Eq)

cuantos_doc :: [Persona] -> Cargo -> Int
cuantos_doc [] _ = 0 
cuantos_doc (x:xs) cargo | (x == Docente cargo) = 1 + cuantos_doc xs cargo
                         | otherwise = cuantos_doc xs cargo


--Ejercicio 4)
primerElemento::[a] -> Maybe a
primerElemento [] = Nothing
primerElemento xs = Just (head xs)


--Ejercicio 5)

data Cola = VaciaC | Encolada Persona Cola

atender :: Cola -> Maybe Cola
atender VaciaC = Nothing  
atender (Encolada persona cola) = Just cola

encolar :: Persona -> Cola ->  Cola
encolar p VaciaC = Encolada p VaciaC
encolar p (Encolada person cola) = Encolada person (encolar p cola)

busca :: Cola -> Cargo -> Maybe Persona
busca VaciaC _ = Nothing
busca (Encolada persona cola) cargo | persona == (Docente cargo) = Just persona
                                    | otherwise = busca cola cargo

--Ejercicio 6)
--Ejercicio 7)