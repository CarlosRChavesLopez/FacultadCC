module Proceso (Procesador, AT(Nil,Tern), RoseTree(Rose), Trie(TrieNodo), foldAT, foldRose, foldTrie, procVacio, procId, procCola, procHijosRose, procHijosAT, procRaizTrie, procSubTries, unoxuno, sufijos, inorder, preorder, postorder, preorderRose, hojasRose, ramasRose, caminos, palabras, ifProc,(++!), (.!)) where

-- import Test.HUnit


--Definiciones de tipos

type Procesador a b = a -> [b]


-- Árboles ternarios
data AT a = Nil | Tern a (AT a) (AT a) (AT a) deriving Eq
--E.g., at = (Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil))
--Es es árbol ternario con 1 en la raíz, y con sus tres hijos 2, 3 y 4.

-- RoseTrees
data RoseTree a = Rose a [RoseTree a] deriving Eq
--E.g., rt = (Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []])
--es el RoseTree con 1 en la raíz y 4 hijos (2, 3, 4 y 5)

-- Tries
data Trie a = TrieNodo (Maybe a) [(Char, Trie a)] deriving Eq
-- E.g., t = (TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])])
-- es el Trie Bool de que tiene True en la raíz, tres hijos (a, b, y c), y, a su vez, "b" tiene como hijo al "a" el cual tiene como hijo al "d".


-- Definiciones de Show

instance Show a => Show (RoseTree a) where
    show = showRoseTree 0
      where
        showRoseTree :: Show a => Int -> RoseTree a -> String
        showRoseTree indent (Rose value children) =
            replicate indent ' ' ++ show value ++ "\n" ++
            concatMap (showRoseTree (indent + 2)) children

instance Show a => Show (AT a) where
    show = showAT 0
      where
        showAT :: Show a => Int -> AT a -> String
        showAT _ Nil = replicate 2 ' ' ++ "Nil"
        showAT indent (Tern value left middle right) =
            replicate indent ' ' ++ show value ++ "\n" ++
            showSubtree (indent + 2) left ++
            showSubtree (indent + 2) middle ++
            showSubtree (indent + 2) right

        showSubtree :: Show a => Int -> AT a -> String
        showSubtree indent subtree =
            case subtree of
                Nil -> replicate indent ' ' ++ "Nil\n"
                _   -> showAT indent subtree

instance Show a => Show (Trie a) where
    show = showTrie ""
      where
        showTrie :: Show a => String -> Trie a -> String
        showTrie indent (TrieNodo maybeValue children) =
            let valueLine = case maybeValue of
                                Nothing -> indent ++ "<vacío>\n"
                                Just v  -> indent ++ "Valor: " ++ show v ++ "\n"
                childrenLines = concatMap (\(c, t) -> showTrie (indent ++ "  " ++ [c] ++ ": ") t) children
            in valueLine ++ childrenLines


--Ejercicio 1
procVacio :: Procesador a b
procVacio _ = []

procId :: Procesador a a
procId p = [p]

procCola :: Procesador [a] a
procCola (x:xs) = xs
procCola [] = []

procHijosRose :: Procesador (RoseTree a) (RoseTree a)
procHijosRose (Rose r hijos) = hijos

procHijosAT :: Procesador (AT a) (AT a)
procHijosAT at = case at of
  Nil -> []
  (Tern r i m d) -> [i,m,d]

procRaizTrie :: Procesador (Trie a) (Maybe a)
procRaizTrie (TrieNodo m d) = [m]

procSubTries :: Procesador (Trie a) (Char, Trie a)
procSubTries (TrieNodo m d) = d


--Ejercicio 2

foldAT :: (a -> b -> b -> b -> b) -> b -> AT a -> b
foldAT cTern cNil at = case at of
  Nil -> cNil
  (Tern r i c d) -> cTern r (recu i) (recu c) (recu d)
  where recu = foldAT cTern cNil

foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose fRose (Rose n hijos) = fRose n (map rec hijos)
  where rec = foldRose fRose

foldTrie :: (Maybe a -> [(Char, b)] -> b) -> Trie a -> b
foldTrie fTrie (TrieNodo r hijos) = fTrie r (map rec hijos)
  where rec (c, t) = (c, foldTrie fTrie t)

--Ejercicio 3
unoxuno :: Procesador [a] [a]
unoxuno = foldr (\x rec -> (:) [x] rec) []

sufijos :: Procesador [a] [a]
sufijos = foldr (\x rec -> (x : head rec) : rec) [[]]

--Ejercicio 4
preorder :: Procesador (AT a) a
preorder = foldAT (\rr ri rc rd -> rr:(ri ++ rc ++ rd)) []

inorder :: Procesador (AT a) a
inorder = foldAT (\rr ri rc rd -> ri ++ (rc ++ (rr : rd))) []

postorder :: Procesador (AT a) a
postorder = foldAT (\rr ri rc rd -> (ri ++ rc ++ rd) ++ [rr]) []

--Ejercicio 5

preorderRose :: Procesador (RoseTree a) a
preorderRose = foldRose (\r xs -> r : concat xs)

hojasRose :: Procesador (RoseTree a) a
hojasRose = foldRose (\n rec -> if null rec then [n] else concat rec)

--ramasRose = undefineds
--ramasRose :: Procesador (RoseTree a) [a]
--ramasRose = foldRose (\n rec -> if null rec then [[n]] else concat (map (n :) rec))

--ramasRose :: RoseTree a -> [[a]]
--ramasRose (Rose n []) = [[n]]  -- Caso base: Si no hay hijos, solo la rama con el nodo actual
--ramasRose (Rose n hijos) = concat (map (\h -> map (n : ) (ramasRose h)) hijos)

-- foldRose :: (a -> [b] -> b) -> RoseTree a -> b
-- foldRose fRose (Rose n hijos) = fRose n (map rec hijos)
--     where rec = foldRose fRose

ramasRose :: Procesador (RoseTree a) [a]
ramasRose = foldRose (\n rec -> if null rec then [[n]] else map ((:) n) (concat rec))

--Ejercicio 6s

-- fTrie :: Maybe a -> [(Char, b)] -> c
-- fTrie a hijos = (map rec hijos)
--   where rec (c', hijos') | hijos' == [] = [c']:[]
--                         | otherwise = [c']:map((:)c') hijos'

-- caminos :: Trie a -> [[Char]]
-- caminos = foldTrie (\ a hijos -> map (\(c', hijos') -> if hijos'==[] then [c']:[] else [c']:map(:c') hijos') hijos) 


caminos :: Trie a -> [[Char]]
caminos trie = "" : foldTrie (\a hijos -> if null hijos then [] else concat (map rec hijos)) trie
  where rec (c,h) | null h = [[c]]
                  | otherwise = [c]:map (c :) h


--TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]
--(f) (Just True) [('a',(f) (Just True) []), ('b',(f) Nothing [('a',(f) (Just True) [('d',(f) Nothing [])])]), ('c',(f) (Just True) [])]
--(f) (Just True) [('a',(f) (Just True) []), ('b',(f) Nothing [('a',(f) (Just True) [('d', [] )])]), ('c',(f) (Just True) [])]
--(f) (Just True) [('a',(f) (Just True) []), ('b',(f) Nothing [('a',[('d', [] )])]), ('c',(f) (Just True) [])]





--Ejercicio 7
-- palabras :: Trie a -> [[Char]]
-- palabras (TrieNodo v hij) = 
--   (case v of 
--     Nothing -> []
--     Just a -> [""]
--   ) ++ foldTrie 
--       (\t hijos -> 
--         if hijos==[] then [] 
--         else case t of
--         Nothing -> concat (map (\(c,h) -> if h==[] then [] else map((:)c) h) hijos)
--         Just a -> concat (map rec hijos)) (TrieNodo v hij)
--         where rec (c,h) | h==[] = [c]:[]
--                         | otherwise = [c]:map((:)c) h

palabras :: Trie a -> [[Char]]
palabras (TrieNodo v hij) =
  (case v of
    Nothing -> []
    Just a -> [""]
  ) ++ snd (foldTrie
  (\t hijos ->
  if null hijos then (t,[])
  else (t,concatMap (\ (c,(b,h)) ->
    case b of
  Nothing -> (if null h then [] else map (c :) h)
  Just a  -> (if null h then [[c]] else [c]:map (c :) h)) hijos)) (TrieNodo v hij))


--Ejercicio 8
-- 8.a)
esNil :: AT a -> Bool
esNil Nil = True
esNil (Tern r i c d) = False

ifProc :: (a->Bool) -> Procesador a b -> Procesador a b -> Procesador a b
ifProc f p1 p2 = \a -> if f a then p1 a else p2 a

-- 8.b)
(++!) :: Procesador a b -> Procesador a b -> Procesador a b
(++!) p1 p2 = \a -> p1 a ++ p2 a

-- 8.c)
(.!) :: Procesador b c -> Procesador a b -> Procesador a c
(.!) p1 p2 = \a -> concat (map p1 (p2 a))

--Ejercicio 9
-- Se recomienda poner la demostración en un documento aparte, por claridad y prolijidad, y, preferentemente, en algún formato de Markup o Latex, de forma de que su lectura no sea complicada.


{-Tests-}

-- main :: IO Counts
-- main = do runTestTT allTests

-- allTests = test [ -- Reemplazar los tests de prueba por tests propios
--   "ejercicio1" ~: testsEj1,
--   "ejercicio2" ~: testsEj2,
--   "ejercicio3" ~: testsEj3,
--   "ejercicio4" ~: testsEj4,
--   "ejercicio5" ~: testsEj5,
--   "ejercicio6" ~: testsEj6,
--   "ejercicio7" ~: testsEj7,
--   "ejercicio8a" ~: testsEj8a,
--   "ejercicio8b" ~: testsEj8b,
--   "ejercicio8c" ~: testsEj8c
--   ]

-- testsEj1 = test [ -- Casos de test para el ejercicio 1
--   procVacio (Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil))
--     ~=? ([] :: [Int])
--   ,
--   procVacio (Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []])
--     ~=? ([] :: [Int])
--   ,
--   procVacio (TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])])
--     ~=? ([] :: [Char])
--   ,
--   procVacio [1,3]         -- Pruebo procVacio con diferentes estructuras, todas deben dar lista vacía
--     ~=? ([] :: [Int])     -- Para que los test pasen se debe especificar algún tipo de lista vacía
--   ,
--   procId (Tern 1 (Tern 2 (Tern 3 Nil Nil Nil) Nil Nil) Nil (Tern 4 Nil Nil Nil))
--     ~=? [(Tern 1 (Tern 2 (Tern 3 Nil Nil Nil) Nil Nil) Nil (Tern 4 Nil Nil Nil))]
--   ,
--   procId (Rose 1 [Rose 2 [Rose 7 []], Rose 3 [], Rose 4 []])
--     ~=? [(Rose 1 [Rose 2 [Rose 7 []], Rose 3 [], Rose 4 []])]
--   ,
--   procId (TrieNodo (Nothing) [('a', TrieNodo (Nothing) []), ('b', TrieNodo Nothing [('a', TrieNodo Nothing [('d', TrieNodo (Just True) [])])])])
--     ~=? [(TrieNodo (Nothing) [('a', TrieNodo (Nothing) []), ('b', TrieNodo Nothing [('a', TrieNodo Nothing [('d', TrieNodo (Just True) [])])])])]
--   ,
--   procId 1      -- Pruebo procId con distintas estructuras
--     ~=? [1]     -- Todas deben dar lo mismo pero dentro de una lista
--   ,
--   procCola [Tern 4 Nil Nil Nil, Tern 2 (Tern 3 Nil Nil Nil) Nil Nil, Tern 1 Nil Nil Nil]
--     ~=? [Tern 2 (Tern 3 Nil Nil Nil) Nil Nil, Tern 1 Nil Nil Nil]
--   ,
--   procCola [Rose 5 [Rose 1 []], Rose 4 [Rose 2 [Rose 1 []], Rose 3 []], Rose 9 []]
--     ~=? [Rose 4 [Rose 2 [Rose 1 []], Rose 3 []], Rose 9 []]
--   ,
--   procCola [TrieNodo Nothing [('n', TrieNodo (Nothing) [])], TrieNodo Nothing [('g', TrieNodo (Just True) [('h', TrieNodo (Just True) [])])], TrieNodo (Just True) [('t', TrieNodo (Just True) [])]]
--     ~=? [TrieNodo Nothing [('g', TrieNodo (Just True) [('h', TrieNodo (Just True) [])])], TrieNodo (Just True) [('t', TrieNodo (Just True) [])]]
--   ,
--   procCola [[1,2], [3,9,7], [9,7,15]]   -- Hago una lista de las diferentes estructuras
--     ~=? [[3,9,7], [9,7,15]]             -- Para comprobar que impriman la cola
--   ,
--   procHijosRose (Rose 1 [Rose 2 [Rose 7 [Rose 9 []]], Rose 3 [], Rose 4 []])
--     ~=? [Rose 2 [Rose 7 [Rose 9 []]], Rose 3 [], Rose 4 []]
--   ,
--   procHijosRose (Rose 1 [])             -- Pruebo Rose con unos cuantos hijos, y otro sin hijos 
--     ~=? []
--   ,
--   procHijosAT (Tern 1 (Tern 2 (Tern 3 Nil Nil Nil) Nil Nil) Nil (Tern 4 Nil Nil Nil))
--     ~=? [Tern 2 (Tern 3 Nil Nil Nil) Nil Nil, Nil, Tern 4 Nil Nil Nil]
--   ,
--   procHijosAT (Tern 1 Nil Nil Nil)
--     ~=? [Nil, Nil, Nil]
--   ,
--   procHijosAT Nil
--     ~=? ([] :: [AT Int])        -- Pruebo HijosAt con varios hijos, sin hijos, y sin raiz
--   ,
--   procRaizTrie (TrieNodo (Nothing) [('a', TrieNodo (Nothing) []), ('b', TrieNodo Nothing [('a', TrieNodo Nothing [('d', TrieNodo (Just True) [])])])])
--     ~=? [Nothing]
--   ,
--   procRaizTrie (TrieNodo (Just True) [('a', TrieNodo (Nothing) []), ('b', TrieNodo Nothing [('a', TrieNodo Nothing [('d', TrieNodo (Just True) [])])])])
--     ~=? [Just True]           -- Caso Raiz, pruebo dos raices distintas
--   , 
--   procSubTries (TrieNodo (Nothing) [('a', TrieNodo (Nothing) []), ('b', TrieNodo Nothing [('a', TrieNodo Nothing [('d', TrieNodo (Just True) [])])])])
--     ~=? [('a', TrieNodo (Nothing) []), ('b', TrieNodo Nothing [('a', TrieNodo Nothing [('d', TrieNodo (Just True) [])])])]
--   ,
--   procSubTries (TrieNodo (Nothing) [])
--     ~=? ([] :: [(Char, Trie (Maybe Bool))])       -- Caso subTries, pruebo con varios subTries y cuando está vacio.
--   ]

-- testsEj2 = test [ -- Casos de test para el ejercicio 2
--   foldAT (\rr ri rc rd -> rr + ri + rc + rd) 0 (Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil (Tern 4 Nil Nil Nil)) Nil)
--     ~=? 10 --Sum Nodos
--   ,
--   foldRose (\n rec -> n + sum rec) (Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []])
--     ~=? 15 --Sum Nodos
--   ,
--   foldTrie (\n hijos -> 1 + sum (map snd hijos)) (TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])])
--     ~=? 6 --Contar Nodos
--   ]

-- testsEj3 = test [ -- Casos de test para el ejercicio 3
--   unoxuno [1,2,3,4,5]
--     ~=? [[1], [2], [3], [4], [5]]
--   ,
--   unoxuno []
--     ~=? ([] :: [[Int]])   -- Probamos unoxuno con varios elementos, y con uno solo
--   ,
--   sufijos "hola"
--     ~=? ["hola", "ola", "la", "a", ""]
--   ,
--   sufijos ""
--     ~=? [""]            -- Probamos sufijos con varios elementos y con uno solo
--   ]

-- testsEj4 = test [ -- Casos de test para el ejercicio 4
--   preorder (Tern 16 (Tern 1 (Tern 9 Nil Nil Nil) (Tern 7 Nil Nil Nil) (Tern 2 Nil Nil Nil)) (Tern 14 (Tern 0 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 6 Nil Nil Nil)) (Tern 10 (Tern 8 Nil Nil Nil) (Tern 5 Nil Nil Nil) (Tern 4 Nil Nil Nil)))
--     ~=? [16, 1, 9, 7, 2, 14, 0, 3, 6, 10, 8, 5, 4]   
--   ,   
--   postorder (Tern 16 (Tern 1 (Tern 9 Nil Nil Nil) (Tern 7 Nil Nil Nil) (Tern 2 Nil Nil Nil)) (Tern 14 (Tern 0 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 6 Nil Nil Nil)) (Tern 10 (Tern 8 Nil Nil Nil) (Tern 5 Nil Nil Nil) (Tern 4 Nil Nil Nil)))
--     ~=? [9, 7, 2, 1, 0, 3, 6, 14, 8, 5, 4, 10, 16]   
--   ,
--   inorder (Tern 16 (Tern 1 (Tern 9 Nil Nil Nil) (Tern 7 Nil Nil Nil) (Tern 2 Nil Nil Nil)) (Tern 14 (Tern 0 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 6 Nil Nil Nil)) (Tern 10 (Tern 8 Nil Nil Nil) (Tern 5 Nil Nil Nil) (Tern 4 Nil Nil Nil)))
--     ~=? [9, 7, 1, 2, 0, 3, 14, 6, 16, 8, 5, 10, 4]                          
--   ]

-- testsEj5 = test [ -- Casos de test para el ejercicio 5
--   preorderRose (Rose 1 [Rose 2 [], Rose 3 [Rose 6 []], Rose 4 [], Rose 5 [Rose 7 [Rose 8 [], Rose 9 []]]])
--     ~=? [1, 2, 3, 6, 4, 5, 7, 8, 9]
--   ,

--   hojasRose (Rose 1 [Rose 2 [], Rose 3 [Rose 6 []], Rose 4 [], Rose 5 [Rose 7 [Rose 8 [], Rose 9 []]]])
--     ~=? [2, 6, 4, 8, 9]
--   ,

--   ramasRose (Rose 1 [Rose 2 [], Rose 3 [Rose 6 []], Rose 4 [], Rose 5 [Rose 7 [Rose 8 [], Rose 9 []]]])
--     ~=? [[1,2],[1,3,6],[1,4],[1,5,7,8],[1,5,7,9]]
--   ]

-- testsEj6 = test [
--   caminos (TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])])
--     ~=? ["","a","b","ba","bad","c"]
--   ,
--   caminos (TrieNodo (Nothing) [('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])])])
--     ~=? ["","b","ba","bad"]
--   ,
--   caminos (TrieNodo (Nothing) [])   -- Probamos camino con el ejemplo dado en la consigna, con un ejemplo distinto y con un Trie vacio
--     ~=? [""]                        
--   ]

-- testsEj7 = test [ -- Casos de test para el ejercicio 7
--   palabras (TrieNodo Nothing [])
--     ~=? []
--   ,
--   palabras (TrieNodo Nothing [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])])
--     ~=? ["a", "ba", "c"]
--   ,
--   palabras (TrieNodo (Just True) [('a', TrieNodo (Just True) [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])])
--     ~=? ["","a","aa","ba","c"]
--   ]

-- testsEj8a = test [ -- Casos de test para el ejercicio 7
--   True         -- Caso de test 1 - expresión a testear
--     ~=? True                                          -- Caso de test 1 - resultado esperado
--   ]
-- testsEj8b = test [ -- Casos de test para el ejercicio 7
--   True         -- Caso de test 1 - expresión a testear
--     ~=? True                                          -- Caso de test 1 - resultado esperado
--   ]
-- testsEj8c = test [ -- Casos de test para el ejercicio 7
--   True         -- Caso de test 1 - expresión a testear
--     ~=? True                                          -- Caso de test 1 - resultado esperado
--   ]
