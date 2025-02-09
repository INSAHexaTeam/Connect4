# Connect4
Réalisation d'un puissance 4 en Prolog


## 🧠 Explication des IA
### Minimax
L'algorithme **Minimax** est utilisé pour modéliser un jeu à somme nulle. Il évalue les mouvements possibles en maximisant le score du joueur et en minimisant celui de l'adversaire. Cet algorithme parcourt tous les coups possibles pour choisir le meilleur.

### Variantes d'IA utilisées
- **IA Aléatoire** :  
  - Cette IA place un jeton dans une colonne choisie au hasard parmi celles qui ne sont pas pleines.  
  - Elle ne prend pas en compte l'état du plateau ni les actions de l'adversaire.  
  - Implémentée dans `aleatoire.pl`.

- **Minimax Basique** :  
  - Utilise l'algorithme **Minimax** avec une profondeur de 3 pour évaluer les meilleurs coups.  
  - Simule les mouvements possibles en maximisant le score du joueur et en minimisant celui de l’adversaire.  
  - Évalue l’état du plateau en attribuant des scores aux situations favorables (victoire, blocage).  
  - Implémentée dans `minimax.pl`.

- **Minimax Défensif** :  
  - Fonction d'amélioration de **Minimax**, qui accorde une priorité à la défense.  
  - Avant de chercher un coup gagnant, il détecte si l’adversaire a une opportunité de victoire au prochain tour et joue pour bloquer cette menace.  
  - Si aucune menace n'est détectée, il applique l'algorithme **Minimax** classique.  
  - Implémentée dans `minimax_defensive.pl`.

- **Minimax avec Poids de Colonnes** :  
  - Fonction d'amélioration de **Minimax**, intégrant une pondération des colonnes en fonction de leur position sur le plateau.  
  - Donne une priorité aux colonnes centrales (souvent plus stratégiques pour créer des alignements).  
  - Combine l’évaluation classique avec une pondération spécifique par colonne.  
  - Implémentée dans `minimax_poids_colonnes.pl`.
