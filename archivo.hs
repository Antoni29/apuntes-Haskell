import System.IO
import Data.List (sort)

main = do
	contents <- readFile "readme.txt"
	
--	putStr contents
--	let palabras = lines contents
--	print palabras
--	print (sort palabras)
--	let txt = unlines (sort palabras)
--	writeFile "datosOrd.txt" txt

--  Es lo mismo que lo anterior:
	writeFile "datosOrd.txt" (unlines (sort(lines contents)))