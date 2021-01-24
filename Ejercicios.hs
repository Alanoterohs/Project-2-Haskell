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
