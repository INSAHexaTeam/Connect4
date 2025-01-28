% filepath: /Users/isalinefoissey/Connect4/gestion/affichage.pl
% Afficher le plateau de jeu
:- module(affichage, [afficher_plateau/1, transpose/2]).

afficher_plateau(Plateau) :-
    remplir_plateau(Plateau, PlateauRempli),  % Compléter les colonnes pour une longueur uniforme
    transpose(PlateauRempli, Lignes),        % Transformer colonnes -> lignes
    reverse(Lignes, LignesAffichees),        % Afficher de bas en haut
    maplist(afficher_ligne, LignesAffichees),
    writeln("1 2 3 4 5 6 7").                % Afficher les numéros des colonnes en bas

remplir_plateau([], []).
remplir_plateau([Colonne|Cols], [ColonneRemplie|PlateauRempli]) :-
    remplir_colonne(Colonne, ColonneRemplie),
    remplir_plateau(Cols, PlateauRempli).

remplir_colonne(Colonne, ColonneRemplie) :-
    length(Colonne, Taille),
    Manque is 6 - Taille,
    length(Espaces, Manque),
    maplist(=(' '), Espaces),  % Créer une liste de cases vides
    append(Colonne, Espaces, ColonneRemplie).

% Afficher une ligne
afficher_ligne(Ligne) :-
    maplist(afficher_case, Ligne),
    nl.

% Afficher une case
afficher_case(Case) :-
    (Case = ' ' -> write('.'); write(Case)),
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
