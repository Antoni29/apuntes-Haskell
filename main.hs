


main::IO()

main = do
	putStrLn "Numero Decimal?"
	decimal <- getLine
	let bin = toBin (read decimal)
	putStrLn bin
	print (toBin2 (read decimal))
	putStrLn decimal

toBin :: Int -> [Char]
toBin 0 = ['0']
toBin 1 = ['1']
toBin n
    | n `mod` 2 == 0 = toBin (n `div` 2) ++ ['0']
    | otherwise = toBin (n `div` 2) ++ ['1']

toBin2 :: Int -> [Int]
toBin2 0 = [0]
toBin2 1 = [1]
toBin2 n
     | n `mod` 2 == 0 = toBin2 (n `div` 2) ++ [0]
     | otherwise = toBin2 (n `div` 2) ++ [1]