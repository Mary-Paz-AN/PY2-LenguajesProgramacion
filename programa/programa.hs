{-# LANGUAGE BlockArguments #-}
import System.Exit (exitSuccess)

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


{-verificarUsuario
Verifica si el id del usuario existe o no-}
verificarUsuario :: Bool
verificarUsuario = True

{-Opciones Operativas
Submenú para las opciones operativas-}
opcionesOperativas :: IO()
opcionesOperativas = do
    let usuario = verificarUsuario

    if usuario then
        do
            putStrLn "\n--Opciones Operativas--"
            putStrLn "1. Crear y Mostrar mobiliario de sala"
            putStrLn "2. Cargar sala de reunión"
            putStrLn "3. Mostrar salas de reunión"
            putStrLn "4. Informe de reservas"
            putStrLn "5. Volver"
            print "Ingrese la opcion deseada: "
            opcion <- getLine

            case opcion of
                "1" -> do
                        putStrLn "Cargando mobiliario"
                        opcionesOperativas
                "2" -> do
                        putStrLn "Cargando salas"
                        opcionesOperativas
                "3" -> do
                        putStrLn "Mostrando salas"
                        opcionesOperativas
                "4" -> do
                        putStrLn "Informe de reservas"
                        opcionesOperativas
                "5" -> putStrLn "Volviendo al Menú Principal..."
                _   -> do
                        putStrLn "Opcion invalida. Vuelva a intentarlo."
                        menuPrincipal
    else 
        do
            putStrLn "El id de usuario puesto no es valido"
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
                    opcionesOperativas
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