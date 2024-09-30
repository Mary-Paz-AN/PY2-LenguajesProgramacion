import System.Exit (exitSuccess)
import Control.Exception (catch, bracket, IOException)
import Data.List.Split (splitOn)  {-Descargar libreria, instrucciones en la documentación-}
import Data.List (group, sort)
import Data.Time (parseTimeM, defaultTimeLocale, Day, formatTime, parseTimeOrError) {-Descargar libreria, instrucciones en la documentación-}
import System.IO
    ( Handle,
      hClose,
      hIsEOF,
      hGetLine,
      hPutStrLn,
      openFile,
      IOMode(AppendMode, ReadMode) )
import System.IO.Error (isEOFError,isDoesNotExistError)
import Text.Read


{-Data structures y types-}
data Mobiliario = Mobiliario {codigoM::Int, nombreMobiliario::String, descripcion::String, tipo::String} deriving (Show)
data Sala = Sala {codigoS::Int, nombreSala::String, edificio::String, piso::Int, ubicacion::String, capacidad::Int} deriving (Show)
data MobiliarioSala = MobiliarioSala {codigoSala::Int, codigoMobiliario::Int} deriving (Show)
data Reserva = Reserva {idReserva::Int, idUsuario::String, idSala::Int, fecha::String, cantidad::Int} deriving (Show)

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


{-leerLineas
Lee cada linea del archivo para convertirla-}
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


{-esEntero
Verificia si un string ado es entero o no-}
esEntero :: String -> Bool
esEntero input = case readMaybe input :: Maybe Int of
        Just _ -> True
        Nothing -> False


{-validarFecha
Verifica si el formato dela fecha es correcto-}
validarFecha :: String -> IO Bool
validarFecha fecha = do
    let maybeFecha = parseTimeM True defaultTimeLocale "%Y-%m-%d" fecha :: Maybe Day
    case maybeFecha of
        Just _  -> return True
        Nothing -> return False


{-verificarReserva
Verifica si el id de la reserva existe o no-}
verificarReserva :: Reservas -> Int -> IO Bool
verificarReserva reservas codigo = do
    let esReserva = any (\r -> idReserva r == codigo) reservas
    return esReserva


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


{-verificarUsuario
Verifica si el id del usuario existe o no-}
verificarUsuario :: String -> IO Bool
verificarUsuario usuario = do
    contenido <- leerArchivo "archivos/usuarios.txt"
    let esUsuario = any (elem usuario) contenido
    return esUsuario


{-getNombreUsuario
Por medio del id del Usuario devuelve el nombre de este-}
getNombreUsuario :: String -> IO String
getNombreUsuario idUsuario = do
        contenido <- leerArchivo "archivos/usuarios.txt"
        let usuario = (head [x | x <- contenido, head x == idUsuario]) !! 1
        return usuario

{-getPuestoUsuario
Por medio del id del Usuario devuelve el puesto de este-}
getPuestoUsuario :: String -> IO String
getPuestoUsuario idUsuario = do
        contenido <- leerArchivo "archivos/usuarios.txt"
        let puesto = (head [x | x <- contenido, head x == idUsuario]) !! 2
        return puesto


{-getMobiliarioSala
devuelve una lista de MobiliarioSala para guardar en memoria-}
getMobiliarioSala:: [String] -> MobiliarioSala
getMobiliarioSala [codigoS, codigoM] =
    MobiliarioSala { codigoSala = read codigoS, codigoMobiliario = read codigoM }


{-getSala
devuelve una lista de Sala para guardar en memoria-}
getSala:: [String] -> Sala
getSala [codigo, nombre, edificio, piso, ubicacion, capacidad] =
    Sala { codigoS = read codigo, nombreSala = nombre, edificio = edificio, piso = read piso, ubicacion = ubicacion, capacidad = read capacidad }


{-getReserva
devuelve una lista de Reserva para guardar en memoria-}
getReserva:: [String] -> Reserva
getReserva [codigo, codUsuario, codSala, fecha, cantidad] =
    Reserva { idReserva = read codigo, idUsuario = codUsuario, idSala = read codSala, fecha = fecha, cantidad = read cantidad }


{-reservaToString
convierte la info de reserva en un string-}
reservaToString :: Reserva -> String
reservaToString Reserva { idReserva, idUsuario, idSala, fecha, cantidad } =
    show idReserva ++ "," ++ idUsuario ++ "," ++ show idSala ++ "," ++ fecha ++ "," ++ show cantidad


{-guardarResrvas
Reescribe el archivo de reservas-}
guardarResrvas :: FilePath -> Reservas -> IO ()
guardarResrvas ruta reservas = writeFile ruta (unlines $ map reservaToString reservas)

{-cancelarReserva
Por medio del id de la reserva se va a eliminar la reserva del archivo y de memoria. Se devolvera la lista de reservas nueva-}
cancelarReserva :: Reservas -> IO Reservas
cancelarReserva reservas = do
        putStrLn "Ingrese el id de la reserva:"
        code <- getLine

        let intCode = (read code :: Int)
        esReserva <- verificarReserva reservas intCode

        if esReserva then do
                let nuevasReservas = filter (\x -> idReserva x /= intCode) reservas
                guardarResrvas "archivos/reservas.txt" nuevasReservas
                putStrLn "La reserva fue cancelada exitosamente..."
                return nuevasReservas
        else do
                putStrLn "El codigo ingresado no es válido"
                cancelarReserva reservas


{-mostrarInfoReserva
Imprime en pantalla la informacion de las reserva-}
mostrarInfoReserva :: Reserva -> IO ()
mostrarInfoReserva reserva = do
        nombreUsuario <- getNombreUsuario (idUsuario reserva)

        putStrLn $ "\nCódigo: " ++ show (idReserva reserva)
        putStrLn $ "Reserva hecha por el usuario: " ++ nombreUsuario
        putStrLn $ "Cédula del usuario: " ++ idUsuario reserva
        putStrLn $ "Código de la sala reservada: " ++ show (idSala reserva)
        putStrLn $ "Fecha: " ++ fecha reserva
        putStrLn $ "Capacidad reservada: " ++ show (cantidad reserva) ++ " personas"


{-consultarReserva
Muestra la información general de la reserva dependiendo del id de reserva dado por el usuario-}
consultarReserva:: Reservas -> IO ()
consultarReserva [] = putStrLn "\nAún no se han hecho reservas."
consultarReserva reservas = do
        putStrLn "Ingrese el id de la reserva:"
        code <- getLine

        let intCode = (read code :: Int)
        esReserva <- verificarReserva reservas intCode

        if esReserva then do
                let reserva = head (filter (\x -> idReserva x == intCode) reservas)
                mostrarInfoReserva reserva
        else do
                putStrLn "El codigo ingresado no es válido"
                consultarReserva reservas


{-preguntarXUsuario
Pregunta por el usuario para crear la reserva. Verifica si el usuario existe o no.-}
preguntarXUsuario :: IO String
preguntarXUsuario = do
        putStrLn "\nIngrese el id de usuario:"
        code <- getLine

        existe <- verificarUsuario code
        if existe then 
                return code
        else do
                putStrLn "\nEl usuario ingresado no existe."
                putStrLn "Porfavor ingrese uno que sea válido."
                preguntarXUsuario


{-preguntarXFecha
Pregunta por la fecha y valida que su formato sea correcto. Si lo es devuelve la fecha ingresada-}
preguntarXFecha :: IO String
preguntarXFecha = do
        putStrLn "\nIngrese la fecha de la reserva (YYYY-MM-DD):"
        fecha <- getLine

        esValida <- validarFecha fecha
        if esValida then do
                return fecha
        else do
                putStrLn "\nEl formato de la fecha no es correcto."
                putStrLn "Porfvor vueva a intentarlo."
                preguntarXFecha


{-preguntarXSala
Pregunta por el id de la sala. Verifica el formato de la entraa y si la sal existe. Si todo es correcto deviuelve el id de la sala-}
preguntarXSala :: Salas -> IO Int
preguntarXSala salas = do
        putStrLn "\nIngrese el id de la sala:"
        code <- getLine

        if esEntero code then do
                let intCode = read code
                if intCode > 0 then do
                        existe <- verificarSala salas intCode
                        if existe then 
                                return intCode
                        else do
                                putStrLn "\nEl id de la sala ingresada no existe."
                                putStrLn "Porfavor vuelva a ingresarlo"
                                preguntarXSala salas
                else do
                        putStrLn "\nPorfavor ingrese un número mayor a 0."
                        preguntarXSala salas
        else do
                putStrLn "\nPorfavor ingrese un número mayor a 0."
                preguntarXSala salas


{-preguntarXCantidad
Pregunta por la cantidad para la reserva. Verifica si su formato es correcto. Si lo es devuelve la cantidad.-}
preguntarXCantidad :: IO Int
preguntarXCantidad = do
        putStrLn "\nIngrese el número de personas para la reserva:"
        cantidad <- getLine

        if esEntero cantidad then do
                let intCantidad = read cantidad
                if intCantidad > 0 then 
                        return intCantidad
                else do
                        putStrLn "\nPorfavor ingrese un número mayor a 0."
                        preguntarXCantidad
        else do
                putStrLn "\nPorfavor ingrese un número mayor a 0."
                preguntarXCantidad


{-verificarDatos
Verifica que la sala este disponible en la fecha dada. Tambien consulta si la cantidad dada es válida.-}
verificarDatos :: Reservas -> Salas -> String -> Int -> Int -> IO Bool
verificarDatos reservas salas fechaUser sala cantidad = do
        let fechaSala = any (\x -> idSala x == sala && fecha x == fechaUser) reservas

        if fechaSala then do
                putStrLn "\nLa sala no esta disponible en la fecha dada."
                return False
        else do
                --Consigue la capacidad de la sala con el id de sala dado por usuario
                let capacidadSala = capacidad (head (filter (\x -> codigoS x == sala) salas))

                if cantidad > capacidadSala then do
                        putStrLn "\nEl número de personas es demasiado."
                        putStrLn ("Esta sala tiene una capcidad de " ++ show capacidadSala ++ ".")
                        return False
                else 
                        return True


{-gestionDeReservas
Se encarga de pregntarle al usuario informacion de una reserva para crearla-}
gestionDeReservas :: Reservas -> Salas -> IO Reservas
gestionDeReservas reservas salas = do
        let codigo = succ (idSala (last reservas))

        usuario <- preguntarXUsuario
        fecha <- preguntarXFecha
        sala <- preguntarXSala salas
        cantidad <- preguntarXCantidad

        exitoReserva <- verificarDatos reservas salas fecha sala cantidad

        if exitoReserva then do
                let strReserva = show codigo ++ "," ++  usuario ++ "," ++ show sala ++ "," ++ fecha ++ "," ++ show cantidad
                guardado <- escribirAppendArchivo "archivos/reservas.txt" strReserva

                if guardado then do
                        let nuevaReserva = Reserva { idReserva = codigo, idUsuario = usuario, idSala = sala, fecha = fecha, cantidad = cantidad }
                        mostrarInfoReserva nuevaReserva
                        putStrLn "\nLa reserva fue creada con exito..."

                        return (reservas ++ [nuevaReserva])
                else do
                        putStrLn "\nHubo un error al guardar la información"
                        putStrLn "Porfavor vuelva a intentarlo."
                        gestionDeReservas reservas salas
        else do
                putStrLn "\nLa creación de la reserva no fue posible."
                putStrLn "Porfavor vuelva a intentarlo."
                return reservas

preguntarFecha :: IO String
preguntarFecha = do
        putStrLn "\nIngrese la fecha  (YYYY-MM-DD):"
        fecha <- getLine

        esValida <- validarFecha fecha
        if esValida then do
                return fecha
        else do
                putStrLn "\nEl formato de la fecha no es correcto."
                putStrLn "Porfvor vueva a intentarlo."
                preguntarFecha
{-Consultar disponibilidad
Funcion para consultar la disponibilidad de una sala-}
consultarDispo :: Reservas -> Salas -> IO Reservas
consultarDispo reservas salas = do
        putStrLn "\n--Consultas--"
        putStrLn "1. Por fecha"
        putStrLn "2. Por Rango de fechas"
        putStrLn "3. Volver"
        putStrLn "Ingrese la opcion deseada:"
        opcion <- getLine

        case opcion of
            "1" -> do
                fechaConsultada <- preguntarFecha 
                let salasDisponibles = salasDisponiblesEnFecha reservas salas fechaConsultada 
                putStrLn $ "Salas disponibles en " ++ fechaConsultada ++ ":"
                mapM_ mostrarSalaConsulta salasDisponibles 
                consultarDispo reservas salas 

            "2" -> do
                putStrLn "\nFecha Inicial: "
                fechaInicio <- preguntarFecha
                putStrLn "\nFecha Final: "
                fechaFin <- preguntarFecha
                consultarPorRango reservas salas fechaInicio fechaFin
                consultarDispo reservas salas
            "3" -> do
                putStrLn "Volviendo al Menu de Opciones Generales..."
                return reservas
            _   -> do
                putStrLn "Opcion invalida. Vuelva a intentarlo."
                consultarDispo reservas salas

{-Mostrar Sala Consulta
Funcion que muestra el nombre y capacidad de una sala-}
mostrarSalaConsulta :: Sala -> IO ()
mostrarSalaConsulta sala = putStrLn $ nombreSala sala ++ " (Capacidad: " ++ show (capacidad sala) ++ ")"

{-Salas disponibles fecha
Funcion que devuelve una lista de salas disponibles en una fecha en especifica-}
salasDisponiblesEnFecha :: Reservas -> Salas -> String -> [Sala]
salasDisponiblesEnFecha reservas salas fechaConsultada =
    let salasReservadas = [idSala r | r <- reservas, fecha r == fechaConsultada]
    in filter (\s -> codigoS s `notElem` salasReservadas) salas

{-Generador de fechas
Funcion que genera un arreglo de fechas en un rango especifico-}
generarFechasRango :: String -> String -> [String]
generarFechasRango inicio fin = map (formatTime defaultTimeLocale "%Y-%m-%d")
                                [parseTimeOrError True defaultTimeLocale "%Y-%m-%d" inicio ::
                                Day .. parseTimeOrError True defaultTimeLocale "%Y-%m-%d" fin]

{-Consultar salas por rango
Funcion que genera un arreglo de salas disponibles en un rango de fechas-}                               
consultarPorRango :: Reservas -> Salas -> String -> String -> IO ()
consultarPorRango reservas salas fechaInicio fechaFin = do
    let fechas = generarFechasRango fechaInicio fechaFin
    mapM_ (\fecha -> do
            let salasDisponibles = salasDisponiblesEnFecha reservas salas fecha
            putStrLn $ "\nFecha: " ++ fecha
            putStrLn "Salas disponibles:"
            mapM_ mostrarSalaConsulta salasDisponibles
          ) fechas

{-Modificacion de Reserva
Funcion para modificar una reserva almacenada en el sistema-}
modificarReserva :: Reservas -> Salas -> IO Reservas
modificarReserva reservas salas = do
        putStrLn "Ingrese el id de la reserva:"
        code <- getLine
        let intCode = (read code :: Int)
        esReserva <- verificarReserva reservas intCode

        if esReserva then do
                let reservaOriginal = head (filter (\r -> idReserva r == intCode) reservas)
                putStrLn "¿Desea modificar la sala? (S/N)"
                modificarSala <- getLine
                nuevaSala <- if modificarSala == "S" then do
                        putStrLn "Ingrese el nuevo identificador de sala:"
                        sala <- getLine
                        return (read sala :: Int)
                else return (idSala reservaOriginal)

                putStrLn "¿Desea modificar la fecha? (S/N)"
                modificarFecha <- getLine
                nuevaFecha <- if modificarFecha == "S" then
                        preguntarXFecha
                else return (fecha reservaOriginal)

                putStrLn "¿Desea modificar la cantidad de personas? (S/N)"
                modificarCantidad <- getLine
                nuevaCantidad <- if modificarCantidad == "S" then do
                        putStrLn "Ingrese la nueva cantidad de personas:"
                        cantidad <- getLine
                        return (read cantidad :: Int)
                else return (cantidad reservaOriginal)

                let nuevasReservas = filter (\x -> idReserva x /= intCode) reservas
                exitoReserva <- verificarDatos nuevasReservas salas nuevaFecha nuevaSala nuevaCantidad

                if exitoReserva then do
                        
                        let nuevaReserva = Reserva { 
                                idReserva = idReserva reservaOriginal,   
                                idUsuario = idUsuario reservaOriginal,   
                                idSala = nuevaSala,                      
                                fecha = nuevaFecha,                      
                                cantidad = nuevaCantidad                 
                        }
                        mostrarInfoReserva nuevaReserva
                        putStrLn "\nLa reserva fue modificada con exito..."

                        let listaActualizada = nuevasReservas ++ [nuevaReserva]
                        guardarResrvas "archivos/reservas.txt" listaActualizada
                        return listaActualizada
                else do
                        putStrLn "\nLa modificacion de la reserva no fue posible."
                        putStrLn "Porfavor vuelva a intentarlo."
                        return reservas
        else do
                putStrLn "El codigo ingresado no es válido"
                modificarReserva reservas salas

{-Opciones Generales
Sub menu para mostrar las opciones generales-}
opcionesGenerales :: Reservas -> Salas -> IO()
opcionesGenerales reservas salas =
    do
        putStrLn "\n--Opciones Generales--"
        putStrLn "1. Gestión de reserva"
        putStrLn "2. Consultar reserva"
        putStrLn "3. Cancelar reserva"
        putStrLn "4. Modificar reserva"
        putStrLn "5. Consulta de disponibilidad de sala"
        putStrLn "6. Volver"
        putStrLn "Ingrese la opcion deseada:"
        opcion <- getLine

        case opcion of
            "1" -> do
                    reservas' <- if null salas then do
                                        putStrLn "Todavia no hay salas existentes."
                                        putStrLn "Porfavor, cree una y vuelva a intentarlo."
                                        return reservas
                                else do
                                        gestionDeReservas reservas salas
                    opcionesGenerales reservas' salas
            "2" -> do
                    if null reservas then do
                        putStrLn "\nTodavia no hay reservas hechas."
                        putStrLn "Porfavor cree una antes de consultar."
                    else do
                        consultarReserva reservas
                    opcionesGenerales reservas salas
            "3" -> do
                    reservas' <- if null reservas then do
                                        putStrLn "\nTodavia no hay reservas hechas."
                                        putStrLn "Porfavor cree una antes de cancelar."
                                        return reservas
                                else do
                                        cancelarReserva reservas
                    opcionesGenerales reservas' salas
            "4" -> do
                    reservas' <- if null reservas then do
                                        putStrLn "\nTodavia no hay reservas hechas."
                                        putStrLn "Porfavor cree una antes de modificar."
                                        return reservas
                                else do
                                        modificarReserva reservas salas
                    opcionesGenerales reservas' salas
            "5" -> do
                    reservas' <- if null reservas then do
                                        putStrLn "\nTodavia no hay reservas hechas."
                                        return reservas
                                else do
                                        consultarDispo reservas salas
                    opcionesGenerales reservas' salas
            "6" -> do
                    putStrLn "Volviendo al Menú Principal..."
                    menuPrincipal
            _   -> do
                    putStrLn "Opcion invalida. Vuelva a intentarlo."
                    opcionesGenerales reservas salas


{-mostrarMobiliarios
Muestra los mobiliarios en memoria-}
mostrarMobiliarios:: Mobiliarios ->  IO ()
mostrarMobiliarios [] = putStrLn ""
mostrarMobiliarios (x:xs) = do
        putStrLn $ "Código: " ++ show (codigoM x)
        putStrLn $ "Nombre: " ++ nombreMobiliario x
        putStrLn $ "Descripción: " ++ descripcion x
        putStrLn $ "Tipo: " ++ tipo x ++ "\n"
        mostrarMobiliarios xs


{-mobiliarioXCodigo
Imprime en pantalla el mobiliario por codigo dado-}
mobiliarioXCodigo :: Int -> Mobiliarios -> IO ()
mobiliarioXCodigo code mobiliarios = do
        let mobiliario = head (filter (\x -> codigoM x == code) mobiliarios)

        putStrLn $ "Código: " ++ show (codigoM mobiliario)
        putStrLn $ "Nombre: " ++ nombreMobiliario mobiliario
        putStrLn $ "Descripción: " ++ descripcion mobiliario
        putStrLn $ "Tipo: " ++ tipo mobiliario ++ "\n"


{-preguntaConfirmacion
Pregunta si desea continuar con la accion-}
preguntaConfirmacion :: IO Bool
preguntaConfirmacion = do
        putStrLn "Desea agregar otro mobiliario (Y/N):"
        respuesta <- getLine

        case respuesta of
                "Y" -> return True
                "N" -> return False
                _ -> do
                        putStrLn "\nOpción no valida. Vuelva a intentarlo"
                        preguntaConfirmacion


{-preguntarXMobiliario
Pregunta por el mobiliario que va a añadir a la sala. Y guarda la información-}
preguntarXMobiliario :: Int -> MobiliariosSala ->  Mobiliarios -> IO MobiliariosSala
preguntarXMobiliario 0 ms mobi = do
        putStrLn "Mobiliario cargado exitosamente."
        return ms
preguntarXMobiliario codSala ms mobi = do
        putStrLn "Ingrese el codigo del mobiliario:"
        codigoMobi <- getLine

        let intCode = read codigoMobi :: Int
        esMobiliario <- verificarMobiliario mobi intCode

        if esMobiliario then do
                --Guardar el mobiliario con su sala
                guardado <- escribirAppendArchivo "archivos/mobiliarioSalas.txt" (show codSala ++ "," ++ codigoMobi)
                if guardado then do
                        let nuevaMobiSala = MobiliarioSala { codigoSala = codSala, codigoMobiliario = intCode }

                        continuar <- preguntaConfirmacion
                        if continuar then do
                                putStrLn ""
                                preguntarXMobiliario codSala (ms ++ [nuevaMobiSala]) mobi
                        else do
                                preguntarXMobiliario 0 (ms ++ [nuevaMobiSala]) mobi
                else do
                        putStrLn "Hubo un error al guardar el mobiliario de la sala."
                        putStrLn "Porfavor vuelva a intentarlo.\n"
                        preguntarXMobiliario codSala ms mobi
        else do
                putStrLn "El código indicado no es válido."
                putStrLn "Porfavor vuelva a intentarlo.\n"
                preguntarXMobiliario codSala ms mobi


{-cargarMobiliarioSala
Le pide al usuario por el mobiliario a agregar a la nueva sala. Luego guarda en memoria y en un archivo.-}
cargarMobiliarioSala :: Salas -> MobiliariosSala -> Mobiliarios -> IO MobiliariosSala
cargarMobiliarioSala salas ms mobiliarios = do
    let codigo = codigoS (last salas)
    putStrLn "\nElija los mobiliarios a agregar"
    preguntarXMobiliario codigo ms mobiliarios


{-preguntarXPiso
Le pregunta al usuario por el piso en el que se encuentra la sala y verifica si el dato ingresado por el usuario sea correcto-}
preguntarXPiso:: IO String
preguntarXPiso = do
        putStrLn "Ingrese el piso en el que se encuentra la sala:"
        piso <- getLine

        if esEntero piso then
                if piso == "0" then do
                        putStrLn "\nNo existe el piso 0. Porfavor vuelva a ingresarlo."
                        preguntarXPiso
                else do
                        if read piso < 0 then do
                                putStrLn "\nEl número de piso no puede ser negativo. Porfavor vuelva a ingresarlo"
                                preguntarXPiso
                        else return piso
        else do
                putStrLn "\nEl valor ingresado no es un entero. Intente nuevamente."
                preguntarXPiso


{-preguntarXCapacidad
Le pregunta al usuario por la cantidad de la sala y verifica si el dato ingresado por el usuario sea correcto-}
preguntarXCapacidad:: IO String
preguntarXCapacidad = do
        putStrLn "Ingrese la capacidad de la sala:"
        capacidad <- getLine

        if esEntero capacidad then
                if capacidad == "0" then do
                        putStrLn "\nLa catidad no puede ser 0. Porfavor vuelva a ingresarla."
                        preguntarXCapacidad
                else do
                        if read capacidad < 0 then do
                                putStrLn "\nLa cantidad no puede ser negativa. Porfavor vuelva a ingresarla"
                                preguntarXCapacidad
                        else return capacidad
        else do
                putStrLn "\nEl valor ingresado no es un entero. Intente nuevamente."
                preguntarXCapacidad


{-cargarSalas
Le pide al usuario la información para crear una sala. Luego se guarda en memoria y en un archivo y por ultimo, le muestra el codigo y su información-}
cargarSalas:: Salas -> IO Salas
cargarSalas salas = do
        putStrLn "\nIngrese el nombre de la sala:"
        nombre <- getLine

        putStrLn "Ingrese el edificio en el que se encuentra la sala:"
        edificio <- getLine

        piso <- preguntarXPiso

        putStrLn "Ingrese la ubicación en el que se encuentra la sala:"
        ubicacion <- getLine

        capacidad <- preguntarXCapacidad

        let codigo = succ (codigoS (last salas))
        let strCodigo = show codigo

        let strSala = strCodigo ++ "," ++ nombre ++ "," ++ edificio ++ "," ++ piso ++ "," ++ ubicacion ++ "," ++ capacidad

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

{-mostrarinfoSala
Imprime la informacion de la sala-}
mostrarinfoSala :: Int -> Salas -> MobiliariosSala -> Mobiliarios -> IO ()
mostrarinfoSala codigo salas mobiliariosSala mobiliarios = do
        let sala = head (filter (\x -> codigoS x == codigo) salas)

        putStrLn $ "\nCódigo: " ++ show (codigoS sala)
        putStrLn $ "Nombre: " ++ nombreSala sala
        putStrLn $ "Edificio: " ++ edificio sala
        putStrLn $ "Piso: " ++ show (piso sala)
        putStrLn $ "Ubicación: " ++ ubicacion sala
        putStrLn $ "Capacidad: " ++ show (capacidad sala)

        putStrLn "\nMobiliario de la sala:"
        let listMobi = filter (\y -> codigoSala y == codigo) mobiliariosSala
        mapM_ (\z -> mobiliarioXCodigo (codigoMobiliario z) mobiliarios) listMobi


{-mostrarSalas
Muestra la sala dependiendo según el código dado-}
mostrarSala:: Salas ->  MobiliariosSala -> Mobiliarios -> IO ()
mostrarSala []  mobiliariosSala mobiliarios  = putStrLn "\nNo hay salas disponibles."
mostrarSala salas mobiliariosSala mobiliarios = do
        putStrLn "Ingrese el codigo de la sala:"
        code <- getLine

        let intCode = (read code :: Int)
        esSala <- verificarSala salas intCode

        if esSala then do
                mostrarinfoSala intCode salas mobiliariosSala mobiliarios
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
        putStrLn "Ingrese la opcion deseada: "
        opcion <- getLine

        case opcion of
            "1" -> do
                    salas' <- cargarSalas salas
                    putStrLn "\nMobilarios disponibles:"
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


{-mostrarReservas
Muestra toda la informacion de las reservas-}
mostrarReservas :: Reserva -> Salas -> MobiliariosSala -> Mobiliarios -> IO ()
mostrarReservas reserva salas mobiliariosSala mobiliarios = do
        putStr "---Reserva---"
        mostrarInfoReserva reserva
        putStr "---Sala---"
        mostrarinfoSala (idSala reserva) salas mobiliariosSala mobiliarios


{-salaMasUsada
Saca la sala más reservada-}
salaMasUsada :: Reservas -> Salas -> MobiliariosSala -> Mobiliarios -> IO ()
salaMasUsada reservas salas mobiliariosSala mobiliarios = do
        let listaIdSalas = [idSala x | x <- reservas]
        let sala = snd (maximum [(length x, head x) | x <- group (sort listaIdSalas)])
        mostrarinfoSala sala salas mobiliariosSala mobiliarios


{-usuarioMasReservas
Saca el usuario con más reservas hechas-}
usuarioMasReservas :: Reservas -> IO ()
usuarioMasReservas reservas = do
        let listaIdUsuario = [idUsuario x | x <- reservas]
        let idUsuario = snd (maximum [(length x, head x) | x <- group (sort listaIdUsuario)])

        nombreUsuario <- getNombreUsuario idUsuario
        puestoUsuario <- getPuestoUsuario idUsuario
        putStrLn ("Cédula: " ++ idUsuario)
        putStrLn ("Nombre: " ++ nombreUsuario)
        putStrLn ("Puesto: " ++ puestoUsuario)


{-diaMasReservado
Saca el usuario con más reservas hechas-}

{-informeReservas
Da un informe de las reservas hechas-}
informeReservas :: Reservas -> Mobiliarios -> IO ()
informeReservas reservas mobiliarios = do
        putStrLn "\n---------Informe de Reservas---------"

        contenidoSala <- leerArchivo "archivos/salas.txt"
        contenidoMS <- leerArchivo "archivos/mobiliarioSalas.txt"
        let salas = map getSala contenidoSala
        let mobiliariosSala = map getMobiliarioSala contenidoMS

        putStrLn "\n---------Reservas Hechas---------"
        mapM_ (\x -> mostrarReservas x salas mobiliariosSala mobiliarios) reservas

        putStr "\n---------Sala más utilizada---------"
        salaMasUsada reservas salas mobiliariosSala mobiliarios

        putStrLn "\n---------Usuario con mayor reservas---------"
        usuarioMasReservas reservas

        putStrLn "\n---------Día con mayor cantidad de reservas---------"
        {-numRepetido ::[Int] -> Int
        numRepetido lista = snd (maximum[(length x, head x) | x <- group(sort lista)])-}

{-Opciones Operativas
Submenú para las opciones operativas-}
opcionesOperativas :: Mobiliarios -> IO()
opcionesOperativas mobiliarios = do
        putStrLn "\n--Opciones Operativas--"
        putStrLn "1. Crear y mostrar mobiliario"
        putStrLn "2. Cargar y mostrar sala de reunión"
        putStrLn "3. Informe de reservas"
        putStrLn "4. Volver"
        putStrLn "Ingrese la opcion deseada: "
        opcion <- getLine

        case opcion of
                "1" -> do
                        putStrLn "Mobiliario previamente cargado:"
                        mapM_ print mobiliarios

                        mobiliarios' <- cargarYMostrarMobiliario []
                        opcionesOperativas mobiliarios'
                "2" -> do
                        contenidoSala <- leerArchivo "archivos/salas.txt"
                        contenidoMS <- leerArchivo "archivos/mobiliarioSalas.txt"
                        let salas = map getSala contenidoSala
                        let mobiliarioSala = map getMobiliarioSala contenidoMS

                        cargarMostrarSalas salas mobiliarioSala mobiliarios
                        opcionesOperativas mobiliarios
                "3" -> do
                        contenidoReserva <- leerArchivo "archivos/reservas.txt"
                        let reservas = map getReserva contenidoReserva

                        informeReservas reservas mobiliarios
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
        putStrLn "Ingrese la opcion deseada: "
        opcion <- getLine

        case opcion of
            "1" -> do
                    putStrLn "\nPorfavor ingrese el id de usuario:"
                    usuarioInput <- getLine
                    usuario <- verificarUsuario usuarioInput

                    if usuario then do
                        mobiliarios <- cargarMobiliarioDesdeArchivo "archivos/mobiliarioGuardado.txt"
                        opcionesOperativas mobiliarios
                    else
                        putStrLn "El id de usuario puesto no es valido"

                    menuPrincipal
            "2" -> do
                    contenidoReserva <- leerArchivo "archivos/reservas.txt"
                    let reservas = map getReserva contenidoReserva
                    contenidoSala <- leerArchivo "archivos/salas.txt"
                    let salas = map getSala contenidoSala
                    opcionesGenerales reservas salas
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