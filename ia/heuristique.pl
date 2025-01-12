% Fichier : ia/heuristique.pl

:- module(heuristique, [evaluer_plateau/3]).

% Évaluer le plateau pour un joueur
evaluer_plateau(Plateau, Joueur, Score) :-
    evaluer_alignements(Plateau, Joueur, ScoreJoueur),
    changer_joueur(Joueur, Adversaire),
    evaluer_alignements(Plateau, Adversaire, ScoreAdversaire),
    Score is ScoreJoueur - ScoreAdversaire,
    format("[DEBUG] Plateau évalué. Joueur : ~w, Score : ~w\n", [Joueur, Score]).


% Évaluer une colonne avec pondération pour la centralité
evaluer_colonne(Colonne, Plateau, Joueur, Score) :-
    evaluer_plateau(Plateau, Joueur, Heuristique),
    centralite_colonne(Colonne, Centralite),
    Score is Heuristique + Centralite.

% Centralité : plus la colonne est proche du centre, plus elle est valorisée
centralite_colonne(Colonne, Centralite) :-
    ValeursCentralite = [0, 1, 2, 3, 3, 2, 1], % Pondération par colonne
    nth1(Colonne, ValeursCentralite, Centralite).

% Compter les alignements partiels avec pondérations
evaluer_alignements(Plateau, Joueur, Score) :-
    findall(Val,
        (mouvement_potentiel(Plateau, Joueur, Alignement, Val), Alignement > 1),
        Scores),
    sum_list(Scores, Score).

% Pondération des alignements potentiels
mouvement_potentiel(Plateau, Joueur, Alignement, Valeur) :-
    (Alignement = 2 -> Valeur = 10;       % Deux pions alignés = 10 points
     Alignement = 3 -> Valeur = 50;       % Trois pions alignés = 50 points
     Alignement = 4 -> Valeur = 100),     % Quatre pions = victoire imminente
    verifier_alignement(Plateau, Joueur, Alignement).

% Vérifie les alignements sur lignes, colonnes, et diagonales
verifier_alignement(Plateau, Joueur, Alignement) :-
    transpose_inverse(Plateau, Lignes),
    diagonales(Plateau, Diagonales),
    append(Lignes, Diagonales, ToutesLignes),
    member(Ligne, ToutesLignes),
    sous_liste_partielle([Joueur|_], Ligne, Alignement).

% Vérifie une sous-liste partielle
sous_liste_partielle(Prefixe, Liste, Alignement) :-
    append(_, Suffixe, Liste),
    append(Prefixe, _, Suffixe),
    length(Prefixe, Alignement).

% Transposer les colonnes en lignes dans l'autre sens
transpose_inverse(Plateau, Lignes) :-
    max_length(Plateau, Max),
    transpose_inverse_helper(Plateau, Max, Lignes).

% Trouver la longueur maximale des colonnes
max_length([], 0).
max_length([Col|Cols], Max) :-
    length(Col, Len),
    max_length(Cols, MaxRest),
    Max is max(Len, MaxRest).

% Construire les lignes à partir des colonnes dans l'autre sens
transpose_inverse_helper(_, 0, []).
transpose_inverse_helper(Plateau, N, [Ligne|Lignes]) :-
    N > 0,
    extract_row_reverse(Plateau, Ligne, RestePlateau),
    N1 is N - 1,
    transpose_inverse_helper(RestePlateau, N1, Lignes).

% Extraire une ligne (éléments en tête des colonnes dans l'autre sens)
extract_row_reverse([], [], []).
extract_row_reverse([Col|Cols], [Elem|Ligne], [Reste|RestePlateau]) :-
    reverse(Col, RevCol), % Inverse la colonne
    (RevCol = [Elem|Reste] -> true; (Elem = ' ', Reste = RevCol)),
    extract_row_reverse(Cols, Ligne, RestePlateau).

