--Daniel Alfaro Miranda
import System.IO
import Control.Monad --when

main = promedia

--Funciones auxiliares
getInt :: IO Int
getInt = do
    line <- getLine
    return (read line::Int)


--Ejercicios de la sesion
--1)

adivina :: Int -> IO ()
adivina n = do
    putStr "Adivina el número que he pensado, vamos prueba! : "
    num <- getInt
    if num == n then putStrLn "Has acertado!"
                else if num < n then do
                                    putStrLn "Es mayor"
                                    adivina n
                                else do
                                    putStrLn "Es menor"
                                    adivina n


--2)

numPalabras :: IO ()
numPalabras = do
    putStr "Introduce una linea para saber su numero de palabras:\n--- "
    line <- getLine
    putStrLn $ "Se introdujeron " ++ show (length $ words line) ++ " palabras."


--3)

--a)
palabras :: String -> IO Int
palabras fileIn = do
    contents <- readFile fileIn
    return $ length $ words contents

--b)
palabras' :: IO ()
palabras' = do
    fileIn <- getLine
    numWords <- palabras fileIn
    putStrLn $ "El fichero " ++ fileIn ++ " tiene " ++ show numWords ++ " palabras."

--c)
promedia :: IO ()
promedia = promediaAux 0 0


promediaAux :: Int -> Int -> IO ()
promediaAux sum numElems = do
    newElem <- getInt
    when (newElem /= -1) $ do
        putStrLn $ "Suma: " ++ show (sum + newElem) ++ " , Promedio: " ++ show ((fromIntegral(sum + newElem) / fromIntegral(numElems + 1))) ++ "."
        promediaAux (sum + newElem) (numElems + 1)

