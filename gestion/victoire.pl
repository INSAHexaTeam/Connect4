% Fichier : gestion/victoire.pl

% Vérifier si un joueur a gagné
verifier_victoire(Plateau, Joueur) :-
    ligne_victoire(Plateau, Joueur);
    colonne_victoire(Plateau, Joueur);
    diagonale_victoire(Plateau, Joueur).

% Vérifier la victoire sur une ligne
ligne_victoire(Plateau, Joueur) :-
    transpose(Plateau, Lignes),
    member(Ligne, Lignes),
    sous_liste([Joueur, Joueur, Joueur, Joueur], Ligne).

% Vérifier la victoire sur une colonne
colonne_victoire(Plateau, Joueur) :-
    member(Colonne, Plateau),
    sous_liste([Joueur, Joueur, Joueur, Joueur], Colonne).

% Vérifier la victoire en diagonale
diagonale_victoire(Plateau, Joueur) :-
    diagonales(Plateau, Diagonales),
    member(Diagonale, Diagonales),
    sous_liste([Joueur, Joueur, Joueur, Joueur], Diagonale).

% Trouver toutes les diagonales possibles
diagonales(Plateau, Diagonales) :-
    diagonales_gauche_droite(Plateau, DG),
    diagonales_droite_gauche(Plateau, DD),
    append(DG, DD, Diagonales).

% Diagonales de gauche à droite
diagonales_gauche_droite(Plateau, Diagonales) :-
    findall(Diagonale, diagonale_gauche_droite(Plateau, Diagonale), Diagonales).

% Diagonales de droite à gauche
diagonales_droite_gauche(Plateau, Diagonales) :-
    findall(Diagonale, diagonale_droite_gauche(Plateau, Diagonale), Diagonales).

% Calculer une diagonale de gauche à droite
diagonale_gauche_droite(Plateau, Diagonale) :-
    diagonale_aux(Plateau, 1, Diagonale).

% Calculer une diagonale de droite à gauche
diagonale_droite_gauche(Plateau, Diagonale) :-
    reverse(Plateau, PlateauInverse),
    diagonale_aux(PlateauInverse, 1, Diagonale).

% Extraire les diagonales
diagonale_aux([], _, []).
diagonale_aux([Col|Cols], N, [Elem|Diagonale]) :-
    nth1(N, Col, Elem),
    N1 is N + 1,
    diagonale_aux(Cols, N1, Diagonale).
diagonale_aux([_|Cols], N, Diagonale) :-
    diagonale_aux(Cols, N, Diagonale).

% Vérifier une sous-liste
sous_liste(SousListe, Liste) :-
    append(_, Suffixe, Liste),
    append(SousListe, _, Suffixe).
