% File: ia/minimax.pl

:- module(minimax, [simuler_coup/4, choisir_colonne_minimax/2]).

% Import joueur_peut_jouer/1 from the joueurs module
:- use_module('../gestion/joueurs', [joueur_peut_jouer/1]).

% Simulate a move on the board
% simuler_coup(+Plateau, +Colonne, +Joueur, -NouveauPlateau)
simuler_coup(Plateau, Colonne, Joueur, NouveauPlateau) :-
    nth1(Colonne, Plateau, Col),
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
    minimax(Plateau, 3, true, _, Colonne).  % Depth of 3

% Minimax algorithm
% minimax(+Plateau, +Profondeur, +MaximizingPlayer, -MeilleurScore, -MeilleurCoup)
minimax(Plateau, 0, _, Score, _) :-
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
% minimax_score(+Plateau, +Profondeur, +MaximizingPlayer, +Coup, -Score)
minimax_score(Plateau, Profondeur, MaximizingPlayer, Coup, Score) :-
    simuler_coup(Plateau, Coup, _, NouveauPlateau),
    minimax(NouveauPlateau, Profondeur, MaximizingPlayer, BaseScore, _),
    poids_colonne(Coup, Poids),
    Score is BaseScore + Poids.

% Evaluate the board
% evaluer_plateau(+Plateau, -Score)
evaluer_plateau(Plateau, Score) :-
    (verifier_victoire(Plateau, 'X') -> Score = 100 ;  % AI win
     verifier_victoire(Plateau, 'O') -> Score = -100 ;  % Opponent win
     Score = 0).

% Assign weights to columns
% poids_colonne(+Colonne, -Poids)
poids_colonne(Colonne, Poids) :-
    PoidsList = [0, 1, 2, 3, 2, 1, 0],
    nth1(Colonne, PoidsList, Poids).