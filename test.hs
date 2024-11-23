fattoriale :: Int -> Int
fattoriale 0 = 1
fattoriale n = n * fattoriale(n-1)

main :: IO ()
main = print (fattoriale 4)