% Fichier : gestion/affichage.pl

% Afficher le plateau de jeu
afficher_plateau(Plateau) :-
    transpose(Plateau, Lignes),
    maplist(afficher_ligne, Lignes).

% Afficher une ligne
afficher_ligne(Ligne) :-
    maplist(afficher_case, Ligne),
    nl.

% Afficher une case
afficher_case(Case) :-
    (var(Case) -> write('.'); write(Case)),
    write(' ').

% Transposer le plateau (colonnes -> lignes)
transpose(Plateau, Lignes) :-
    max_length(Plateau, Max),
    transpose_helper(Plateau, Max, Lignes).

% Trouver la longueur maximale des colonnes
max_length([], 0).
max_length([Col|Cols], Max) :-
    length(Col, Len),
    max_length(Cols, MaxRest),
    Max is max(Len, MaxRest).

% Construire les lignes à partir des colonnes
transpose_helper(_, 0, []).
transpose_helper(Plateau, N, [Ligne|Lignes]) :-
    N > 0,
    extract_row(Plateau, Ligne, RestePlateau),
    N1 is N - 1,
    transpose_helper(RestePlateau, N1, Lignes).

% Extraire une ligne (éléments en tête des colonnes)
extract_row([], [], []).
extract_row([Col|Cols], [Elem|Ligne], [Reste|RestePlateau]) :-
    (Col = [Elem|Reste] -> true; (Elem = ' ', Reste = Col)),
    extract_row(Cols, Ligne, RestePlateau).
