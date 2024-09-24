import System.Exit (exitSuccess)
import Control.Exception (catch, bracket, IOException)
import Data.List.Split (splitOn)  {-Descargar libreria, instrucciones en la documentación-}
import System.IO
import System.IO.Error (isEOFError,isDoesNotExistError)

{-Data structures y types-}
data Mobiliario = Mobiliario {codigoM::Int, nombreMobiliario::String, descripcion::String, tipo::String} deriving (Show)
data Sala = Sala {codigoS::Int, nombreSala::String, edificio::String, piso::Int, ubicacion::String, capacidad::Int} deriving (Show)
data MobiliarioSala = MobiliarioSala {codigoSala::Int, codigoMobiliario::Int} deriving (Show)
data Reserva = Reserva {idUsuario::Int, idSala::Int, fecha::String, cantidad::Int} deriving (Show)

type Mobiliarios = [Mobiliario]
type Salas = [Sala]
type MobiliariosSala = [MobiliarioSala]
type Reservas = [Reserva]


manejarErrorArchivo :: IOException -> IO [[String]]
manejarErrorArchivo e
  | isDoesNotExistError e = do
      putStrLn "El archivo no existe. Por favor, verifica la ruta."
      return []
  | otherwise = ioError e


cargarYMostrarMobiliario :: Mobiliarios -> IO Mobiliarios
cargarYMostrarMobiliario mobiliarioActual = do
  putStrLn "Ingresa la ruta del archivo para cargar nuevos ítems:"
  ruta <- getLine
  contenido <- leerArchivo ruta
  if null contenido then do
        putStrLn "No se pudo cargar ningún mobiliario debido a un error."
        return mobiliarioActual
    else do
      let nuevoMobiliario = cargarMobiliario contenido mobiliarioActual
      putStrLn "Mobiliario cargado:"
      mapM_ print nuevoMobiliario

      -- Guardamos el mobiliario actualizado en un archivo
      guardarMobiliario "archivos/mobiliarioGuardado.txt" nuevoMobiliario

      return nuevoMobiliario



-- Cargar nuevos ítems sin duplicar códigos
cargarMobiliario :: [[String]] -> Mobiliarios -> Mobiliarios
cargarMobiliario [] mobiliario = mobiliario
cargarMobiliario (fila:filaRestante) mobiliario =
  let nuevoItem = parseMobiliario fila
  in if any (\item -> codigoM item == codigoM nuevoItem) mobiliario
       then cargarMobiliario filaRestante mobiliario  -- Ignorar duplicados
       else cargarMobiliario filaRestante (nuevoItem : mobiliario)


-- Definición de la función que convierte un Mobiliario en un String
mobiliarioToString :: Mobiliario -> String
mobiliarioToString Mobiliario { codigoM, nombreMobiliario, descripcion, tipo } =
    show codigoM ++ "," ++ nombreMobiliario ++ "," ++ descripcion ++ "," ++ tipo


-- Función para guardar la lista de mobiliario en un archivo
guardarMobiliario :: FilePath -> Mobiliarios -> IO ()
guardarMobiliario ruta mobiliario = writeFile ruta (unlines $ map mobiliarioToString mobiliario)


-- Cargar mobiliario desde un archivo almacenado previamente
cargarMobiliarioDesdeArchivo :: FilePath -> IO Mobiliarios
cargarMobiliarioDesdeArchivo ruta = do
    contenido <- leerArchivo ruta
    let nuevoMobiliario = cargarMobiliario contenido []
    return nuevoMobiliario  -- Asegúrate de retornar el nuevo mobiliario si lo necesitas


parseMobiliario :: [String] -> Mobiliario
parseMobiliario [codigoStr, nombre, desc, tipo] =
  Mobiliario { codigoM = read codigoStr, nombreMobiliario = nombre, descripcion = desc, tipo = tipo }
parseMobiliario _ = error "Formato incorrecto en el archivo"


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
leerArchivo :: FilePath -> IO [[String]]
leerArchivo ruta = catch (do
  handle <- openFile ruta ReadMode
  contenidoLista <- leerLineas handle
  hClose handle
  return contenidoLista)
  manejarErrorArchivo

leerLineas :: Handle -> IO [[String]]
leerLineas handle = do
  eof <- hIsEOF handle
  if eof
    then return []
    else do
      linea <- catch (hGetLine handle) (\e -> if isEOFError e then return "" else ioError e)
      let fila = splitOn "," linea
      rest <- leerLineas handle
      return (fila : rest)


{-escribirAppendArchivo
Añade un string a un archivo dado por una ruta-}
escribirAppendArchivo :: FilePath -> String -> IO Bool
escribirAppendArchivo ruta string =
  catch (do
    handle <- openFile ruta AppendMode
    hPutStrLn handle string
    hClose handle
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

{-verificarSala
Verifica si el id de la sala existe o no-}
verificarSala :: Salas -> Int -> IO Bool
verificarSala salas codigo = do
    let esSala = any (\s -> codigoS s == codigo) salas
    return esSala


{-verificarMobiliario
Verifica si el id del mobiliario existe o no-}
verificarMobiliario :: Mobiliarios -> Int -> IO Bool
verificarMobiliario mobiliarios codigo = do
    let esMobilario = any (\m -> codigoM m == codigo) mobiliarios
    return esMobilario


{-mostrarMobiliarios
Muestra los mobiliarios en memoria-}
mostrarMobiliarios:: Mobiliarios ->  IO ()
mostrarMobiliarios [] = putStrLn ""
mostrarMobiliarios (x:xs) = do
        putStrLn $ "\nCódigo: " ++ show (codigoM x)
        putStrLn $ "Nombre: " ++ nombreMobiliario x
        putStrLn $ "Descripción: " ++ descripcion x
        putStrLn $ "Tipo: " ++ tipo x
        mostrarMobiliarios xs


{-mobiliarioXCodigo
Imprime en pantalla el mobiliario por codigo dado-}
mobiliarioXCodigo :: Int -> Mobiliarios -> IO ()
mobiliarioXCodigo code mobiliarios = do
        let mobiliario = head (filter (\x -> codigoM x == code) mobiliarios)

        putStrLn $ "\nCódigo: " ++ show (codigoM mobiliario)
        putStrLn $ "Nombre: " ++ nombreMobiliario mobiliario
        putStrLn $ "Descripción: " ++ descripcion mobiliario
        putStrLn $ "Tipo: " ++ tipo mobiliario


{-getMobiliarioSala
devuelve una lista de MobiliarioSala para guardar en memoria-}
getMobiliarioSala:: [String] -> MobiliarioSala
getMobiliarioSala [codigoS, codigoM] =
    MobiliarioSala { codigoSala = read codigoS, codigoMobiliario = read codigoM }


{-getSalas
devuelve una lista de Sala para guardar en memoria-}
getSalas:: [String] -> Sala
getSalas [codigo, nombre, edificio, piso, ubicacion, capacidad] =
    Sala { codigoS = read codigo, nombreSala = nombre, edificio = edificio, piso = read piso, ubicacion = ubicacion, capacidad = read capacidad }


{-preguntarXMobiliario
Pregunta por el mobiliario que va a añadir a la sala. Y guarda la información-}
preguntarXMobiliario :: Int -> MobiliariosSala ->  Mobiliarios -> IO MobiliariosSala
preguntarXMobiliario 0 ms mobi = return ms


{-cargarMobiliarioSala
Le pide al usuario por el mobiliario a agregar a la nueva sala. Luego guarda en memoria y en un archivo.-}
cargarMobiliarioSala :: Salas -> MobiliariosSala -> Mobiliarios -> IO MobiliariosSala
cargarMobiliarioSala salas ms mobiliarios = do
    let codigo = succ (codigoS (last salas))
    preguntarXMobiliario codigo ms mobiliarios


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

                return (salas ++ [nuevaSala])
        else do
                putStrLn "Error al añadir la sala. Intente nuevamente."
                cargarSalas salas


{-mostrarSalas
Muestra todas las salas en memoria-}
mostrarSala:: Salas ->  MobiliariosSala -> Mobiliarios -> IO ()
mostrarSala []  mobiliariosSala mobiliarios  = putStrLn "\nNo hay salas disponibles."
mostrarSala salas mobiliariosSala mobiliarios = do
        putStrLn "Ingrese el codigo de la sala:"
        code <- getLine

        let intCode = (read code :: Int)
        esSala <- verificarSala salas intCode

        if esSala then do
                let sala = head (filter (\x -> codigoS x == intCode) salas)

                putStrLn $ "\nCódigo: " ++ show (codigoS sala)
                putStrLn $ "Nombre: " ++ nombreSala sala
                putStrLn $ "Edificio: " ++ edificio sala
                putStrLn $ "Piso: " ++ show (piso sala)
                putStrLn $ "Ubicación: " ++ ubicacion sala
                putStrLn $ "Capacidad: " ++ show (capacidad sala)

                putStrLn "\nMobiliario:"
                let listMobi = filter (\y -> codigoSala y == intCode) mobiliariosSala
                mapM_ (\z -> mobiliarioXCodigo (codigoMobiliario z) mobiliarios) listMobi
        else do
                putStrLn "El codigo ingresado no es válido"
                mostrarSala salas mobiliariosSala mobiliarios


{-CargarMostrarSalas
Submenú para preguntarle si quiere cargar una sala o ver todas las salas-}
cargarMostrarSalas:: Salas -> MobiliariosSala -> Mobiliarios -> IO ()
cargarMostrarSalas salas mobiliariosSala mobiliarios = do
        putStrLn "\n--Salas--"
        putStrLn "1. Cargar Sala"
        putStrLn "2. Mostrar salas"
        putStrLn "3. Volver"
        print "Ingrese la opcion deseada: "
        opcion <- getLine

        case opcion of
            "1" -> do
                    salas' <- cargarSalas salas
                    mostrarMobiliarios mobiliarios
                    mobiliarioSala' <- cargarMobiliarioSala salas' mobiliariosSala mobiliarios
                    cargarMostrarSalas salas' mobiliarioSala' mobiliarios
            "2" -> do
                    mostrarSala salas mobiliariosSala mobiliarios
                    cargarMostrarSalas salas mobiliariosSala mobiliarios
            "3" -> do
                    putStrLn "Volviendo al Opciones Operativas..."
                    opcionesOperativas mobiliarios
            _   -> do
                    putStrLn "Opcion Invalida. Vuelva a intentarlo."
                    cargarMostrarSalas salas mobiliariosSala mobiliarios


{-Opciones Operativas
Submenú para las opciones operativas-}
opcionesOperativas :: Mobiliarios -> IO()
opcionesOperativas mobiliarios = do
        putStrLn "\n--Opciones Operativas--"
        putStrLn "1. Crear y mostrar mobiliario"
        putStrLn "2. Cargar y mostrar sala de reunión"
        putStrLn "3. Informe de reservas"
        putStrLn "4. Volver"
        print "Ingrese la opcion deseada: "
        opcion <- getLine

        case opcion of
                "1" -> do
                        mobiliarioActual <- cargarMobiliarioDesdeArchivo "archivos/mobiliarioGuardado.txt"
                        putStrLn "Mobiliario previamente cargado:"
                        mapM_ print mobiliarioActual

                        mobiliarios' <- cargarYMostrarMobiliario []
                        opcionesOperativas mobiliarios
                "2" -> do
                        mobiliarios' <- if null mobiliarios then do
                                                putStrLn "\nTodavia no se han cargado mobiliarios."
                                                putStrLn "Se usaran los mobiliarios guardados."
                                                cargarMobiliarioDesdeArchivo "archivos/mobiliarioGuardado.txt"
                                        else return mobiliarios

                        contenidoSala <- leerArchivo "archivos/salas.txt"
                        contenidoMS <- leerArchivo "archivos/mobiliarioSalas.txt"
                        let salas = map getSalas contenidoSala
                        let mobiliarioSala = map getMobiliarioSala contenidoMS

                        cargarMostrarSalas salas mobiliarioSala mobiliarios'
                        opcionesOperativas mobiliarios
                "3" -> do
                        putStrLn "Informe de reservas"
                        opcionesOperativas mobiliarios
                "4" -> do
                        putStrLn "Volviendo al Menú Principal..."
                        menuPrincipal
                _   -> do
                        putStrLn "Opcion invalida. Vuelva a intentarlo."
                        opcionesOperativas mobiliarios


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
                        opcionesOperativas []
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