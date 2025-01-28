% filepath: /Users/isalinefoissey/Connect4/test/test_victoire.pl
% Fichier : test_victoire.pl

:- begin_tests(victoire).

% Test de victoire par ligne
test(victoire_ligne) :-
    Plateau = [
        ['X', 'X', 'X', 'X', 'O', 'O', 'O'],
        ['O', 'O', 'O', 'X', 'X', 'X', 'X'],
        ['X', 'X', 'X', 'O', 'O', 'O', 'O'],
        ['O', 'O', 'O', 'X', 'X', 'X', 'X'],
        ['X', 'X', 'X', 'O', 'O', 'O', 'O'],
        ['O', 'O', 'O', 'X', 'X', 'X', 'X']
    ],
    verifier_victoire(Plateau, 'X').

% Test de victoire par colonne
test(victoire_colonne) :-
    Plateau = [
        ['X', 'O', 'X', 'O', 'X', 'O', 'X'],
        ['X', 'O', 'X', 'O', 'X', 'O', 'X'],
        ['X', 'O', 'X', 'O', 'X', 'O', 'X'],
        ['X', 'O', 'X', 'O', 'X', 'O', 'X'],
        ['O', 'X', 'O', 'X', 'O', 'X', 'O'],
        ['O', 'X', 'O', 'X', 'O', 'X', 'O']
    ],
    verifier_victoire(Plateau, 'X').

% Test de victoire par diagonale (gauche à droite)
test(victoire_diagonale_gauche_droite) :-
    Plateau = [
    [['X'], ['O', 'X', 'O', 'X'], ['O', 'X', 'X'], ['X', 'X', 'O', 'X'], ['O'], ['X'], ['O']]
    ],
    verifier_victoire(Plateau, 'X').

% Test de victoire par diagonale (droite à gauche)
test(victoire_diagonale_droite_gauche) :-
    Plateau = [
        ['O', 'O', 'O', 'O', 'O', 'O', 'X'],
        ['O', 'O', 'O', 'O', 'O', 'X', 'O'],
        ['O', 'O', 'O', 'O', 'X', 'O', 'O'],
        ['O', 'O', 'O', 'X', 'O', 'O', 'O'],
        ['O', 'O', 'X', 'O', 'O', 'O', 'O'],
        ['O', 'X', 'O', 'O', 'O', 'O', 'O']
    ],
    verifier_victoire(Plateau, 'X').

:- end_tests(victoire).
