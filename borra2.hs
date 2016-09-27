import System.IO
import System.Directory
import Data.List

main = do
	fh <- openFile "todo.txt" ReadMode
	(arch_tmp, fh_tmp) <- openTempFile "." "temp"
	contenido <- hGetContents fh
	let cont_lineas = lines contenido
	    num_lineas  zipWith (\num linea -> show num ++ " - " ++ linea) [0..] cont_lineas
	putStrLn ("\nArchivo temporal: " ++ arch_tmp)
	putStrLn "\nEstas son las lineas del Archivo de Texto: "
	putStr (unlines num_lineas)
	putStrLn "¿Cual linea quieres borrar?"
	num_texto <- getLine
	let numero = read num_texto
	    nuevo_cont_lineas = delete (cont_lineas !! numero) cont_lineas
	hPutStr fh_tmp $ unlines nuevo_cont_lineas
	hClose fh
	hClose fh_tmp
	removeFile "todo.txt"
	renameFile arch_tmp "todo.txt"
