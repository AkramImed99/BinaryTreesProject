module Arbre where
import Control.Concurrent (threadDelay)
import Test.QuickCheck

   
data Arbre c v = Noeud { coul :: c
                              , val :: v
                              , gauche :: Arbre c v
                              , droite :: Arbre c v }
                | Feuille
                      deriving (Show, Eq, Ord)


mapArbre :: (a -> b) -> (c -> d) -> Arbre a c -> Arbre b d
mapArbre _ _ Feuille         = Feuille
mapArbre f e (Noeud c v g d) = Noeud (f c) (e v) (mapArbre f e g) (mapArbre f e d)


-- Pour hauteur et taille , on peut les implémenter en deux méthodes:
--la première:

hauteur :: Arbre c a -> Int
hauteur Feuille = 0
hauteur (Noeud _ _ g d) = 1 + max (hauteur g) (hauteur d)



taille :: Arbre c a -> Int
taille Feuille = 0
taille (Noeud _ _ g d) = 1 + taille g + taille d

-- La deuxieme qui est demandé dans la question, donc on définit une fonction foldArbre qui sera appellée dans les deux foncts:

foldArbre :: (a -> b -> b -> b) -> b -> Arbre c a -> b
foldArbre _ n Feuille         = n
foldArbre f n (Noeud _ v g d) = f v (foldArbre f n g) (foldArbre f n d)


hauteur' :: Arbre c a -> Int
hauteur' = foldArbre (\_ y z -> 1 + max y z) 0



taille' :: Arbre c a -> Integer
taille' = foldArbre (\_ y z -> 1 + y + z) 0
 
 
 -- La question 4: C'est la meme la fonction foldArbre définie précédement
 

peigneGauche :: [(c,a)] -> Arbre c a
peigneGauche = foldr (\ (c, v) x -> Noeud c v x Feuille) Feuille


prop_hauteurPeigne xs = length xs == hauteur (peigneGauche xs)

-- *Arbre> quickCheck prop_hauteurPeigne
-- +++ OK, passed 100 tests.

-- ça veut dire que le test est réussi pour 100 listes aléatoires. (que la lengueur de la liste passée en param est egale à la longueur de l'arbre peigneGauche)



estComplet :: Arbre c a -> Bool
estComplet Feuille         = True
estComplet (Noeud _ _ g d) = (hauteur g == hauteur d) && estComplet g && estComplet d


-- Question 10
-- l'arbre vide (Feuille) et l'arbre avec 1 seul élément.



          
complet :: Int -> [(c, a)] -> Arbre c a
complet 0 _ = Feuille
complet _ [] = error "Pas d'elements pour creer l'arbre"
complet x t = Noeud c v (complet (x-1) g) (complet (x-1) d)
         where (g, ((c,v):d)) = splitAt (length t `quot` 2) t 
         


repeat' :: a -> [a]
repeat' = iterate id

-- qst 13

tailleListe :: Int -> Int
tailleListe 0 = 0
tailleListe n = 1 + 2 * tailleListe (n-1)

-- ce qui suit ne sert que pour générer de grand nombre d'identifiant de noeuds pour graphviz

nextWords [] = []
nextWords (s:xs) = [s ++ [t] | t <- ['a'..'z']] ++ (nextWords xs)

identifiers = concat (iterate nextWords (map (\x -> [x]) ['a'..'z']))

-- implémentation de base pour l'exercice 11 :
-- createListe = [((),j) | j <- ['a'..]]

-- implémentation améliorée
createListe = [((),j) | j <- identifiers]

createListForComplet n = take (tailleListe n) createListe

-- exemple de création d'un arbre complet :
-- complet 5 (createListForComplet 5)



aplatit :: Arbre c a -> [(c, a)]
aplatit Feuille = []
aplatit (Noeud c a g d) = aplatit g ++ [(c, a)] ++ aplatit d





complet4 :: Arbre String Char
complet4 = complet 4 [("blue", 'a'), ("blue", 'b'), ("blue", 'c'),
                      ("blue", 'd'), ("blue", 'e'), ("blue", 'f'), ("blue", 'g'),
                      ("blue", 'h'), ("blue", 'i'), ("blue", 'j'),
                      ("blue", 'k'), ("blue", 'l'), ("blue", 'm'),
                      ("blue", 'n'), ("blue", 'o')]
                      
                     
-- map snd (aplatit complet4) == "abcdefghijklmno"
-- là j ai une erreur de syntaxe

element :: Eq a => a -> Arbre c a -> Bool
element _ Feuille = False
element a (Noeud _ v g d) = (v == a) || (element a g) || (element a d)



  
charToString :: Char -> String
charToString c = [c]
        

noeud :: (c -> String) -> (a -> String) -> (c, a) -> String
noeud fc fv (c, v) = fv v ++ " [color=" ++ strC ++ ", fontcolor=" ++ strC ++ "]"
                where strC = fc c

      

valNoeud :: Arbre c a -> a
valNoeud Feuille = error "Feuille"
valNoeud (Noeud _ v _ _) = v          

arcs :: Arbre c a -> [(a, a)]
arcs Feuille = []
arcs (Noeud _ _ Feuille Feuille) = []
arcs (Noeud _ v g Feuille) = [(v, valNoeud g)] ++ (arcs g)
arcs (Noeud _ v Feuille d) = [(v, valNoeud d)] ++ (arcs d)
arcs (Noeud _ v g d) = [(v, valNoeud g), (v, valNoeud d)] ++ (arcs g) ++ (arcs d)


                                          
arc :: (a -> String) -> (a, a) -> String
arc f (x, y) = (f x) ++ " -> " ++ (f y)



dotise :: String -> (c -> String) -> (a -> String) -> Arbre c a -> String
dotise t fc fv a = unlines (
                    ["digraph \"" ++ t ++ "\" {",
                     "node [shape=circle]"]
                    ++ (map (noeud fc fv) (aplatit a))
                    ++ (map (arc fv) (arcs a))
                    ++ ["}"]
                )



-- test :
testArbre :: IO ()
testArbre = do
        writeFile "arbre.dot" (dotise "Test dotise " (\_ -> "black") id (complet 4 (createListForComplet 4)))




-- Enfin de la couleur ...!

-- qst 20

elementR :: (Eq a, Ord a) => a -> Arbre c a -> Bool 
elementR v Feuille = False
elementR a (Noeud _ v g d)  | a == v = True
                            | a < v  = elementR a g
                            | a > v  = elementR a d


-- qst 21

data Couleur = R | N
                deriving (Show, Eq)

couleurToString :: Couleur -> String
couleurToString R = "red"
couleurToString N = "black"





--qst 22


equilibre :: Arbre Couleur a -> Arbre Couleur a
equilibre Feuille = Feuille
equilibre (Noeud _ z (Noeud R y (Noeud R x a b) c) d) = Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre (Noeud _ z (Noeud R x a (Noeud R y b c)) d) = Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre (Noeud _ x a (Noeud R z (Noeud R y b c) d)) = Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre (Noeud _ x a (Noeud R y b (Noeud R z c d))) = Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre abr = abr




--qst 23

ajouterValeurArbre v abr = (Noeud N a g d)
                        where (Noeud _ a g d) = ajouterValeurArbre' v abr


ajouterValeurArbre' :: (Eq a, Ord a) => a -> Arbre Couleur a -> Arbre Couleur a
ajouterValeurArbre' v Feuille = (Noeud N v Feuille Feuille) 
ajouterValeurArbre' v (Noeud c a Feuille d) | v < a = Noeud c a (Noeud R v Feuille Feuille) d 
ajouterValeurArbre' v (Noeud c a g Feuille) | v > a = Noeud c a g (Noeud R v Feuille Feuille)  
ajouterValeurArbre' v abr@(Noeud c a g d) | v < a = equilibre (Noeud c a (ajouterValeurArbre v g) d)
                                     | v > a = equilibre (Noeud c a g (ajouterValeurArbre v d))
                                     | otherwise = abr 


-- Les tests :ça fonctionne




