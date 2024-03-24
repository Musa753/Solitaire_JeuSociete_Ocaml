Jeu de solitaire en OCaml

Ce programme implémente un jeu de solitaire en OCaml. Le but du jeu est de retirer tous les pions d'un plateau de jeu en sautant par-dessus d'autres pions, jusqu'à ce qu'il n'en reste plus qu'un.

Le jeu est implémenté à l'aide d'une grille hexagonale, qui est représentée par un tableau de bits de 64 bits. Chaque bit correspond à une position sur le plateau, et un bit à 1 indique qu'il y a un pion à cette position.

Le programme utilise un algorithme de recherche en profondeur pour trouver une solution au jeu. La recherche commence à partir d'un état initial donné, et explore tous les états possibles du jeu en retirant un pion à chaque fois. L'algorithme utilise une table de hachage pour stocker les états déjà explorés, afin d'éviter de les explorer à nouveau.

Il est possible de sauvegarder l'état du jeu dans un fichier CSV, pour pouvoir reprendre une partie ultérieurement.

Installation

Pour utiliser ce programme, il faut d'abord installer OCaml sur votre système. Vous pouvez télécharger la dernière version d'OCaml à partir du site officiel : https://ocaml.org/

Une fois que vous avez installé OCaml, vous pouvez télécharger le code source du programme à partir de ce dépôt Git. Vous pouvez également cloner le dépôt Git en utilisant la commande suivante :

bash

git clone 

Utilisation :

Pour compiler le programme, vous pouvez utiliser la commande make dans le répertoire du projet. Cela créera un exécutable appelé solitaire dans le répertoire courant.

Pour exécuter le programme, vous pouvez lancer l'exécutable solitaire depuis le terminal. Pour que le programme s'exécute, il faudra au préalable choisir la configuration de départ dans la fonction solve (variable ind_hold_empty) qui représentera la case vide à l'état initial.
