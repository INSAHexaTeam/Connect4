% File: ia/minimax.pl

:- module(minimax, [simuler_coup/4, choisir_colonne_minimax/2, minimax/5]).

% Import joueur_peut_jouer/1 from the joueurs module
:- use_module('../gestion/joueurs', [joueur_peut_jouer/1]).

% Import verifier_victoire/2
:- use_module('../gestion/victoire', [verifier_victoire/2]).

% Simulate a move on the board
% simuler_coup(+Plateau, +Colonne, +Joueur, -NouveauPlateau)
simuler_coup(Plateau, Colonne, Joueur, NouveauPlateau) :-
    nth1(Colonne, Plateau, Col),
    length(Col, Len),
    Len < 6,  % Vérifier si la colonne n'est pas pleine
    append(Col, [Joueur], NouvelleCol),
    remplacer_colonne(Plateau, Colonne, NouvelleCol, NouveauPlateau).

% Replace a column in the board
% remplacer_colonne(+Plateau, +Colonne, +NouvelleCol, -NouveauPlateau)
remplacer_colonne(Plateau, Colonne, NouvelleCol, NouveauPlateau) :-
    nth1(Colonne, Plateau, _, Rest),
    nth1(Colonne, NouveauPlateau, NouvelleCol, Rest).

% Minimax logic: choose the optimal column
% choisir_colonne_minimax(+Plateau, -Colonne)
choisir_colonne_minimax(Plateau, Colonne) :-
    minimax(Plateau, 4, true, _, Colonne).  % Depth of 5

% Minimax algorithm
% minimax(+Plateau, +Profondeur, +MaximizingPlayer, -MeilleurScore, -MeilleurCoup)
minimax(Plateau, _, _, Score, _) :-
    verifier_victoire(Plateau, 'X'), !,  % Victoire de l'IA
    Score = 100.

minimax(Plateau, _, _, Score, _) :-
    verifier_victoire(Plateau, 'O'), !,  % Victoire de l'adversaire
    Score = -100.

minimax(Plateau, 0, _, Score, _) :- !,
    evaluer_plateau(Plateau, Score).  % Base case: evaluate the board

minimax(Plateau, Profondeur, true, MeilleurScore, MeilleurCoup) :-
    findall(Coup, joueur_peut_jouer(Coup), Coups),  % Use joueur_peut_jouer/1 from joueurs module
    Profondeur1 is Profondeur - 1,
    maplist(minimax_score(Plateau, Profondeur1, false), Coups, Scores),
    max_list(Scores, MeilleurScore),
    nth0(Index, Scores, MeilleurScore),
    nth0(Index, Coups, MeilleurCoup).

minimax(Plateau, Profondeur, false, MeilleurScore, MeilleurCoup) :-
    findall(Coup, joueur_peut_jouer(Coup), Coups),  % Use joueur_peut_jouer/1 from joueurs module
    Profondeur1 is Profondeur - 1,
    maplist(minimax_score(Plateau, Profondeur1, true), Coups, Scores),
    min_list(Scores, MeilleurScore),
    nth0(Index, Scores, MeilleurScore),
    nth0(Index, Coups, MeilleurCoup).

% Helper: calculate the score for a move
minimax_score(Plateau, Profondeur, true, Coup, Score) :-
    (simuler_coup(Plateau, Coup, 'X', NouveauPlateau) ->  % Tour de l'IA (X)
        minimax(NouveauPlateau, Profondeur, false, Score, _)
    ;   Score = -1000).  % Pénalité si coup invalide

minimax_score(Plateau, Profondeur, false, Coup, Score) :-
    (simuler_coup(Plateau, Coup, 'O', NouveauPlateau) ->  % Tour de l'adversaire (O)
        minimax(NouveauPlateau, Profondeur, true, Score, _)
    ;   Score = 1000).  % Pénalité si coup invalide

% Extraire les diagonales du plateau
extraire_diagonales(Plateau, Diagonales) :-
    extraire_diagonales_principales(Plateau, DiagPrincipales),
    reverse_colonnes(Plateau, PlateauInverse),
    extraire_diagonales_principales(PlateauInverse, DiagSecondaires),
    append(DiagPrincipales, DiagSecondaires, Diagonales).

extraire_diagonales_principales(Plateau, Diagonales) :-
    length(Plateau, NbCol),
    findall(Diag, (
        between(1, NbCol, Start),
        extraire_diagonale(Plateau, Start, 1, Diag),
        length(Diag, Len),
        Len >= 4
    ), Diagonales).

extraire_diagonale(_, _, _, []) :- !.
extraire_diagonale(Plateau, Col, Row, [Elem|Rest]) :-
    nth1(Col, Plateau, Colonne),
    nth1(Row, Colonne, Elem), !,
    ColSuiv is Col + 1,
    RowSuiv is Row + 1,
    extraire_diagonale(Plateau, ColSuiv, RowSuiv, Rest).
extraire_diagonale(_, _, _, []).

% Helper: reverse columns of the board
reverse_colonnes(Plateau, PlateauInverse) :-
    maplist(reverse, Plateau, PlateauInverse).

% Simplification de l'évaluation pour commencer
evaluer_sequence(Sequence, Score) :-
    (subsequence([X,X,X,X], Sequence) -> 
        (X = 'X' -> Score = 100 ; X = 'O' -> Score = -100 ; Score = 0)
    ;
    subsequence([X,X,X], Sequence) -> 
        (X = 'X' -> Score = 5 ; X = 'O' -> Score = -5 ; Score = 0)
    ;
    subsequence([X,X], Sequence) -> 
        (X = 'X' -> Score = 2 ; X = 'O' -> Score = -2 ; Score = 0)
    ;
    Score = 0).

% Helper: check if List1 is a subsequence of List2
subsequence(List1, List2) :-
    append(_, Rest, List2),
    append(List1, _, Rest).

% Evaluate the board
% evaluer_plateau(+Plateau, -Score)
evaluer_plateau(Plateau, Score) :-
    evaluer_lignes(Plateau, ScoreLignes),
    evaluer_colonnes(Plateau, ScoreColonnes),
    evaluer_diagonales(Plateau, ScoreDiagonales),
    Score is ScoreLignes + ScoreColonnes + ScoreDiagonales.


%%Ajout d'une heuristique de base
% Évaluation des alignements de 2 et 3 pions
evaluer_lignes(Plateau, Score) :-
    findall(S, (member(Ligne, Plateau), evaluer_sequence(Ligne, S)), Scores),
    sum_list(Scores, Score).

evaluer_colonnes(Plateau, Score) :-
    transpose(Plateau, PlateauTranspose),
    findall(S, (member(Colonne, PlateauTranspose), evaluer_sequence(Colonne, S)), Scores),
    sum_list(Scores, Score).

evaluer_diagonales(Plateau, Score) :-
    extraire_diagonales(Plateau, Diagonales),
    findall(S, (member(Diag, Diagonales), evaluer_sequence(Diag, S)), Scores),
    sum_list(Scores, Score).
