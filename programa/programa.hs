import System.Exit (exitSuccess)
import Control.Exception (catch, IOException)
import Data.List.Split (splitOn)  {-Descargar libreria, instrucciones en la documentación-}
import System.IO

{-Data structures y types-}
data Mobiliario = Mobiliario {codigoM::Int, nombreMobiliario::String, descripcion::String, tipo::String} deriving (Show)
data Sala = Sala {codigoS::Int, nombreSala::String, edificio::String, piso::Int, ubicacion::String, capacidad::Int} deriving (Show)
data MobiliarioSala = MobiliarioSala {codigoSala::Int, codigoMobiliario::Int} deriving (Show)
data Reserva = Reserva {idUsuario::Int, idSala::Int, fecha::String, cantidad::Int} deriving (Show)

type Mobiliarios = [Mobiliario]
type Salas = [Sala]
type MobiliariosSala = [MobiliarioSala]
type Reservas = [Reserva]

{-manejarError
Maneja los errores al abrir el archivo-}
manejarError :: IOException -> IO [[String]]
manejarError _ = do
    putStrLn "Ocurrio un error a leer el archivo."
    return []


{-manejarErrorEscribir
Maneja los errores al escribir en el archivo-}
manejarErrorEscribir :: IOError -> IO Bool
manejarErrorEscribir e = do
    putStrLn $ "Error al escribir en el archivo: " ++ show e
    return False


{-leerArchivo
Lee un archivo dado por una ruta y devuelve el contenido en una lista de string-}
leerArchivo :: String -> IO [[String]]
leerArchivo ruta = catch (do
    contenedor <- openFile ruta ReadMode
    contenido <- hGetContents contenedor

    let lineas = splitOn "\n" contenido
        contenidoLista = map (splitOn ",") lineas

    return contenidoLista
    ) manejarError


{-escribirAppendArchivo
Añade un string a un archivo dado por una ruta-}
escribirAppendArchivo :: String -> String -> IO Bool
escribirAppendArchivo ruta string = catch (do
    appendFile ruta string
    return True) manejarErrorEscribir


{-verificarUsuario
Verifica si el id del usuario existe o no-}
verificarUsuario :: String -> IO Bool
verificarUsuario usuario = do
    contenido <- leerArchivo "archivos/usuarios.txt"
    let esUsuario = any (elem usuario) contenido
    return esUsuario


{-Opciones Generales
Sub menu para mostrar las opciones generales-}
opcionesGenerales :: IO()
opcionesGenerales =
    do
        putStrLn "\n--Opciones Generales--"
        putStrLn "1. Gestión de reserva"
        putStrLn "2. Consultar reserva"
        putStrLn "3. Cancelar reserva"
        putStrLn "4. Modificar reserva"
        putStrLn "5. Consulta de disponibilidad de sala"
        putStrLn "6. Volver"
        print "Ingrese la opcion deseada: "
        opcion <- getLine

        case opcion of
            "1" -> do
                    putStrLn "Gestionando Reserva"
                    opcionesGenerales
            "2" -> do
                    putStrLn "Consultando Reserva"
                    opcionesGenerales
            "3" -> do
                    putStrLn "Cancelando Reserva"
                    opcionesGenerales
            "4" -> do
                    putStrLn "Modificando Reserva"
                    opcionesGenerales
            "5" -> do
                    putStrLn "Consultando disponibilidad de sala"
                    opcionesGenerales
            "6" -> putStrLn "Volviendo al Menú Principal..."
            _   -> do
                    putStrLn "Opcion invalida. Vuelva a intentarlo."
                    opcionesGenerales


{-getSalas
devuelve una lista de Sala para guardar en memoria-}
getSalas:: [String] -> Sala
getSalas [codigo, nombre, edificio, piso, ubicacion, capacidad] =
    Sala { codigoS = read codigo, nombreSala = nombre, edificio = edificio, piso = read piso, ubicacion = ubicacion, capacidad = read capacidad }


{-cargarSalas
Le pide al usuario la información para crear una sala. Luego se guarda en memoria y en un archivo y por ultimo, le muestra el codigo y su información-}
cargarSalas:: Salas -> IO Salas
cargarSalas salas = do
        putStrLn "\nIngrese el nombre de la sala:"
        nombre <- getLine

        putStrLn "Ingrese el edificio en el que se encuentra la sala:"
        edificio <- getLine

        putStrLn "Ingrese el piso en el que se encuentra la sala:"
        piso <- getLine

        putStrLn "Ingrese la ubicación en el que se encuentra la sala:"
        ubicacion <- getLine

        putStrLn "Ingrese la capacidad de la sala:"
        capacidad <- getLine

        let codigo = succ (codigoS (last salas))
        let strCodigo = show codigo

        let strSala = "\n" ++ strCodigo ++ "," ++ nombre ++ "," ++ edificio ++ "," ++ piso ++ "," ++ ubicacion ++ "," ++ capacidad

        guardado <- escribirAppendArchivo "archivos/salas.txt" strSala

        if guardado
                then do
                putStrLn "Sala añadida exitosamente\n"
                putStrLn $ "Codigo de sala: " ++ strCodigo
                putStrLn $ "Nombre de sala: " ++ nombre
                putStrLn $ "Edificio de sala: " ++ edificio
                putStrLn $ "Piso de sala: " ++ piso
                putStrLn $ "Ubicación de sala: " ++ ubicacion
                putStrLn $ "Capacidad de sala: " ++ capacidad

                let nuevaSala = Sala { codigoS = codigo, nombreSala = nombre, edificio = edificio, piso = read piso, ubicacion = ubicacion, capacidad = read capacidad }

                -- Devuelve la nueva lista de salas
                return (salas ++ [nuevaSala])
        else do
                putStrLn "Error al añadir la sala. Intente nuevamente."
                cargarSalas salas


{-mostrarSalas
Muestra todas las salas en memoria-}
mostrarSalas:: Salas -> IO ()
mostrarSalas [] = putStrLn "\nNo hay salas disponibles."
mostrarSalas (x:xs) = do
        putStrLn $ "\nCódigo: " ++ show (codigoS x)
        putStrLn $ "Nombre: " ++ nombreSala x
        putStrLn $ "Edificio: " ++ edificio x
        putStrLn $ "Piso: " ++ show (piso x)
        putStrLn $ "Ubicación: " ++ ubicacion x
        putStrLn $ "Capacidad: " ++ show (capacidad x)
        mostrarSalas xs


{-CargarMostrarSalas
Submenú para preguntarle si quiere cargar una sala o ver todas las salas-}
cargarMostrarSalas:: IO ()
cargarMostrarSalas = do
        contenido <- leerArchivo "archivos/salas.txt"
        let salas = map getSalas contenido

        putStrLn "\n--Salas--"
        putStrLn "1. Cargar Sala"
        putStrLn "2. Mostrar salas"
        putStrLn "3. Volver"
        print "Ingrese la opcion deseada: "
        opcion <- getLine

        case opcion of
            "1" -> do
                    salas' <- cargarSalas salas
                    cargarMostrarSalas
            "2" -> do
                    mostrarSalas salas
                    cargarMostrarSalas
            "3" -> do
                    putStrLn "Volviendo al Opciones Operativas..."
                    opcionesOperativas
            _   -> do
                    putStrLn "Opcion Invalida. Vuelva a intentarlo."
                    cargarMostrarSalas


{-Opciones Operativas
Submenú para las opciones operativas-}
opcionesOperativas :: IO()
opcionesOperativas = do
        putStrLn "\n--Opciones Operativas--"
        putStrLn "1. Crear y mostrar mobiliario"
        putStrLn "2. Cargar y mostrar sala de reunión"
        putStrLn "3. Informe de reservas"
        putStrLn "4. Volver"
        print "Ingrese la opcion deseada: "
        opcion <- getLine

        case opcion of
                "1" -> do
                        putStrLn "Cargando mobiliario"
                        opcionesOperativas
                "2" -> do
                        cargarMostrarSalas
                        opcionesOperativas
                "3" -> do
                        putStrLn "Informe de reservas"
                        opcionesOperativas
                "4" -> putStrLn "Volviendo al Menú Principal..."
                _   -> do
                        putStrLn "Opcion invalida. Vuelva a intentarlo."
                        menuPrincipal


{-Menu principal-}
menuPrincipal :: IO ()
menuPrincipal =
    do
        putStrLn "\n--Menú Principal--"
        putStrLn "1. Opciones Operativas"
        putStrLn "2. Opciones Generales"
        putStrLn "3. Salir"
        print "Ingrese la opcion deseada: "
        opcion <- getLine

        case opcion of
            "1" -> do
                    putStrLn "\nPorfavor ingrese el id de usuario:"
                    usuarioInput <- getLine
                    usuario <- verificarUsuario usuarioInput

                    if usuario then
                        opcionesOperativas
                    else
                        putStrLn "El id de usuario puesto no es valido"

                    menuPrincipal
            "2" -> do
                    opcionesGenerales
                    menuPrincipal
            "3" -> do
                    putStrLn "Saliendo del porgrama..."
                    exitSuccess
            _   -> do
                    putStrLn "Opcion Invalida. Vuelva a intentarlo."
                    menuPrincipal


{-FUncion main-}
main :: IO ()
main = menuPrincipal