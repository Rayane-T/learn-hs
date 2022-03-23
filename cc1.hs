-- Écrire la fonction base2 n qui étant donné un nombre entier (décimal) n renvoie son écriture en
-- base 2 sous la forme d’une chaîne de caractères. Exemple : base2 77 renvoie "1001101". Attention à bien
-- écrire les chiffres binaires dans le bon ordre!

base2 n =
  if n<0 then error "L’argument doit être un entier positif"
    else
  if n==0 then "0"
   else
  if n==1 then "1"
     else
       let reste = n ‘mod‘ 2 in
          base2 (n ‘div‘ 2) ++ show reste
          
-- Écrire la fonction base b n qui étant donné un nombre entier b (2 <= b <= 10) et un nombre entier
-- n renvoie l’écriture de n en base b sous la forme d’une chaîne de caractères. Exemple : base 3 77 renvoie
-- "2212", base 2 77 renvoie "1001101". Dans le cas où b < 2 ou bien b > 10, alors la fonction doit renvoyer
-- une erreur.          

base b n =
  if n<0 then error "L’argument doit être un entier positif"
    else
  if n==0 then "0"
    else
  if n<b then show n
    else
      let reste = n ‘mod‘ b in
        base b (n ‘div‘ b) ++ show reste

-- Écrire la fonction scan b s dont le résultat est un booléen, et qui teste si un brin d’ADN b (de
-- longueur quelconque) est présent dans une séquence s. Utiliser la fonction prédéfinie head s dont le résultat est le premier caractère d’une chaîne s, la fonction prédéfinie tail s dont le résultat est la chaîne de
-- caractères s sans son premier caractère, et la fonction prédéfinie length s dont le résultat est le nombre
-- de caractères d’une chaîne s. Exemples : head "TRUC" a pour résultat ’T’, tail "TRUC" a pour résultat
-- "RUC", length "TRUC" a pour résultat 4, et length "" a pour résultat 0. Indication : vous pouvez définir
-- une fonction récursive auxiliaire scan_aux b br s avec 1 argument de plus que la fonction scan b s :
-- l’argument br qui est la partie du brin d’ADN restant à analyser.

s1 = "CCTGGAGGGTGGCCCCACCGGCCGAGACAGCGAGCATATGCAGGAAGCGGCAGGAATAAGGAAAAGCAGC"
s2 = "CTCCTGATGCTCCTCGCTTGGTGGTTTGAGTGGACCTCCCAGGCCAGTGCCGGGCCCCTCATAGGAGAGG"
s3 = "AAGCTCGGGAGGTGGCCAGGCGGCAGGAAGGCGCACCCCCCCAGTACTCCGCGCGCCGGGACAGAATGCC"
s4 = "CTGCAGGAACTTCTTCTGGAAGTACTTCTCCTCCTGCAAATAAAACCTCACCCATGAATGCTCACGCAAG"
scan_aux b br s =
  if length s<length br then
      False
    else
  if length br==0 then
      True
    else
  if head br == head s then
      scan_aux b (tail br) (tail s)
    else
      scan_aux b br (tail s)
scan b s = scan_aux b b s


