% IA simple qui joue aléatoirement
jouer_coup_ia(Plateau, NouveauPlateau) :-
    % Vérifier que le plateau est une liste
    is_list(Plateau),
    % Obtenir la liste des coups valides
    findall(Col, (between(1, 7, Col), coup_valide(Plateau, Col)), CoupsValides),
    % Vérifier qu'il y a des coups valides disponibles
    CoupsValides \= [],
    % Choisir un coup aléatoire parmi les coups valides
    random_member(ColChoisie, CoupsValides),
    % Jouer le coup
    ajouter_pion(Plateau, ColChoisie, 'O', NouveauPlateau).

% Prédicat pour vérifier si un coup est valide
coup_valide(Plateau, Col) :-
    % Vérifier que les arguments sont du bon type
    is_list(Plateau),
    integer(Col),
    % Vérifier les limites
    Col >= 1, Col =< 7,
    % Récupérer la colonne
    nth1(Col, Plateau, Colonne),
    % Vérifier que c'est une liste
    is_list(Colonne),
    % Vérifier la hauteur
    length(Colonne, Hauteur),
    Hauteur < 6.

% Prédicat pour ajouter un pion dans une colonne
ajouter_pion(Plateau, Col, Pion, NouveauPlateau) :-
    % Vérifier les arguments
    is_list(Plateau),
    integer(Col),
    Col >= 1, Col =< 7,
    % Récupérer la colonne
    nth1(Col, Plateau, Colonne),
    % Ajouter le pion à la colonne
    append(Colonne, [Pion], NouvelleColonne),
    % Remplacer l'ancienne colonne par la nouvelle
    length(Plateau, N),
    findall(C, (between(1, N, I),
                (I = Col -> C = NouvelleColonne
                         ; nth1(I, Plateau, C))), NouveauPlateau). 