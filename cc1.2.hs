--1. Écrire la fonction signe qui renvoie le signe d’un nombre décimal.

signe x = if x<0 then "-" else "+"

--2. Écrire la fonction récursive exposant qui renvoie l’exposant d’un nombre décimal positif.

  exposant x =
  if x>=1 && x<10 then 0
                  else if x<1 then exposant (x*10)-1
                              else exposant (x/10)+1
                              
 --3. Écrire la fonction récursive mantisse qui renvoie la mantisse d’un nombre décimal positif.

   
 mantisse x =
    if x>=1 && x<10 then x
        else if x<1 then mantisse (x*10)
                    else mantisse (x/10)
                    
  --4. Écrire la fonction notation_scientifique qui renvoie la notation scientifique d’un nombre décimal
--non nul. On utilisera les fonctions prédéfinies abs qui calcule la valeur absolue d’un nombre, et show
--qui convertit un nombre en une chaîne de caractères. Exemples : abs (-2.3) renvoie 2.3, show 2.3
--renvoie "2.3", et show 23 renvoie "23". Rappel : La concaténation de chaînes de caractères est un
--opérateur prédéfini en HASKELL noté ++. Exemple : "bon"++"jour" renvoie "bonjour".

  notation_scientifique x =
      (signe x)++show(mantisse(abs x))++"x10^"++show(exposant(abs x))
