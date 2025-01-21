:- module(minimax_defensive, [choisir_colonne_minimax_defensive/2]).

% Import joueur_peut_jouer/1 from the joueurs module
:- use_module('../gestion/joueurs', [joueur_peut_jouer/1]).
:- use_module('minimax', [simuler_coup/4, minimax/5]).


% Simulate a move on the board
% simuler_coup_defensive(+Plateau, +Colonne, +Joueur, -NouveauPlateau)
simuler_coup_defensive(Plateau, Colonne, Joueur, NouveauPlateau) :-
    nth1(Colonne, Plateau, Col),
    append(Col, [Joueur], NouvelleCol),
    remplacer_colonne(Plateau, Colonne, NouvelleCol, NouveauPlateau).

% Replace a column in the board
% remplacer_colonne(+Plateau, +Colonne, +NouvelleCol, -NouveauPlateau)
remplacer_colonne(Plateau, Colonne, NouvelleCol, NouveauPlateau) :-
    nth1(Colonne, Plateau, _, Rest),
    nth1(Colonne, NouveauPlateau, NouvelleCol, Rest).


% Minimax algorithm
% minimax_defensive(+Plateau, +Profondeur, +MaximizingPlayer, -MeilleurScore, -MeilleurCoup)
minimax_defensive(Plateau, 0, _, Score, _) :-
    evaluer_plateau(Plateau, Score).  % Base case: evaluate the board

minimax_defensive(Plateau, Profondeur, true, MeilleurScore, MeilleurCoup) :-
    findall(Coup, joueur_peut_jouer(Coup), Coups),  % Use joueur_peut_jouer/1 from joueurs module
    Profondeur1 is Profondeur - 1,
    maplist(minimax_score(Plateau, Profondeur1, false), Coups, Scores),
    max_list(Scores, MeilleurScore),
    nth0(Index, Scores, MeilleurScore),
    nth0(Index, Coups, MeilleurCoup).

minimax_defensive(Plateau, Profondeur, false, MeilleurScore, MeilleurCoup) :-
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
    minimax(NouveauPlateau, Profondeur, MaximizingPlayer, Score, _).

% Evaluate the board
% evaluer_plateau(+Plateau, -Score)
evaluer_plateau(Plateau, Score) :-
    (verifier_victoire(Plateau, 'X') -> Score = 100 ;  % AI win
     verifier_victoire(Plateau, 'O') -> Score = -100 ;  % Opponent win
     Score = 0).  % Neutral


% Check if the opponent has an immediate winning move
% bloquer_si_necessaire(+Plateau, +Joueur, -ColonneBloque)
bloquer_si_necessaire(Plateau, Opponent, ColonneBloque) :-
    % Find all valid moves
    findall(Colonne, joueur_peut_jouer(Colonne), Coups),
    % Check each move to see if it leads to a victory for the opponent
    member(Colonne, Coups),
    simuler_coup(Plateau, Colonne, Opponent, NouveauPlateau),
    verifier_victoire(NouveauPlateau, Opponent),  % Opponent wins with this move
    ColonneBloque = Colonne, !.  % Return the blocking column

% Minimax logic: choose the optimal column
% choisir_colonne_minimax_defensive(+Plateau, -Colonne)
choisir_colonne_minimax_defensive(Plateau, Colonne) :-
    % Assume AI plays as 'O' and the opponent is 'X'
    (   bloquer_si_necessaire(Plateau, 'X', Colonne)  
    ->  true  % If a threat exists, block it directly
    ;   minimax_defensive(Plateau, 3, true, _, Colonne)  % Otherwise, call minimax
    ).


