{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}

--Ejercicio 1
--I.
max2 :: (Float, Float) -> Float
max2 (x,y)
    | x >= y = x
    | otherwise = y

normaVectorial :: (Float, Float) -> Float
normaVectorial (x,y) = sqrt (x^2 + y^2)

subtract' :: Float -> Float -> Float
subtract' = flip (-) 

predecesor :: Float -> Float
predecesor = subtract' 1

evaluarEnCero :: (Float -> b) -> b
evaluarEnCero = \f -> f 0

dosVeces :: (Float -> Float) -> Float -> Float 
dosVeces = \f -> f . f

flipAll :: [a->b->c] -> [b-> a -> c]
flipAll  = map flip 

flipRaro :: b -> (a -> b -> c) -> a -> c 
flipRaro  = flip flip

--II

--currificadas: subtract', predecesor, evaluarEnCero, dosVeces, flipAll, flipRaro
--nocurrificadas: max2, normaVectorial

max2Currificada:: Float -> Float -> Float
max2Currificada x y
    | x >= y = x
    | otherwise = y

normaVectorialCurrificada:: Float -> Float -> Float
normaVectorialCurrificada x y = sqrt (x^2 + y^2)

--Ejercicio 2.
--I.

curry:: ((a,b) -> c) -> (a -> b -> c)
curry f x y = f (x,y)

--II.
uncurry:: (a -> b -> c) -> ((a,b) -> c)
uncurry f (x,y) = f x y

--III.
--duda...

--Ejercicio 3.
--I.

sumFold :: [Int] -> Int
sumFold = foldr (+) 0

