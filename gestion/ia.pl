% IA utilisant l'algorithme MinMax pour Puissance 4
jouer_coup_ia(Plateau, NouveauPlateau) :-
    % Vérifier que le plateau est une liste
    is_list(Plateau),
    % Profondeur de recherche
    Profondeur = 5,
    % Trouver le meilleur coup avec MinMax
    minmax(Plateau, Profondeur, 'O', -10000, 10000, ColChoisie, _),
    % Jouer le coup
    ajouter_pion(Plateau, ColChoisie, 'O', NouveauPlateau).

% Prédicat MinMax avec élagage alpha-beta
minmax(Plateau, 0, Joueur, Alpha, Beta, _, Score) :-
    % Cas de base : évaluer la position
    evaluer_position(Plateau, Joueur, Score).

minmax(Plateau, Profondeur, Joueur, Alpha, Beta, MeilleurCoup, MeilleurScore) :-
    Profondeur > 0,
    % Obtenir tous les coups possibles
    findall(Col, coup_valide(Plateau, Col), CoupsPossibles),
    % Si pas de coups possibles ou victoire, évaluer la position
    (CoupsPossibles = [] -> evaluer_position(Plateau, Joueur, MeilleurScore)
    ; % Sinon, explorer les coups
      ProfondeurSuivante is Profondeur - 1,
      (Joueur = 'O' -> 
          evaluer_max(Plateau, ProfondeurSuivante, CoupsPossibles, Alpha, Beta, MeilleurCoup, MeilleurScore)
      ; evaluer_min(Plateau, ProfondeurSuivante, CoupsPossibles, Alpha, Beta, MeilleurCoup, MeilleurScore)
      )
    ).

% Évaluation pour le joueur MAX (IA)
evaluer_max(Plateau, Profondeur, [Coup|Coups], Alpha, Beta, MeilleurCoup, MeilleurScore) :-
    % Simuler le coup
    ajouter_pion(Plateau, Coup, 'O', NouveauPlateau),
    % Appel récursif pour le joueur adverse
    minmax(NouveauPlateau, Profondeur, 'X', Alpha, Beta, _, Score),
    % Mettre à jour Alpha
    NouvelAlpha is max(Alpha, Score),
    % Continuer l'exploration ou couper (élagage)
    (NouvelAlpha >= Beta ->
        % Élagage Beta
        MeilleurScore = NouvelAlpha,
        MeilleurCoup = Coup
    ; evaluer_max_suite(Plateau, Profondeur, Coups, NouvelAlpha, Beta, Coup, Score, MeilleurCoup, MeilleurScore)
    ).

evaluer_max(_, _, [], _, _, Coup, Score, Coup, Score).

evaluer_max_suite(Plateau, Profondeur, Coups, Alpha, Beta, CoupActuel, ScoreActuel, MeilleurCoup, MeilleurScore) :-
    evaluer_max(Plateau, Profondeur, Coups, Alpha, Beta, CoupSuivant, ScoreSuivant),
    (ScoreSuivant > ScoreActuel ->
        MeilleurCoup = CoupSuivant,
        MeilleurScore = ScoreSuivant
    ; MeilleurCoup = CoupActuel,
      MeilleurScore = ScoreActuel
    ).

% Évaluation pour le joueur MIN (Humain)
evaluer_min(Plateau, Profondeur, [Coup|Coups], Alpha, Beta, MeilleurCoup, MeilleurScore) :-
    % Simuler le coup
    ajouter_pion(Plateau, Coup, 'X', NouveauPlateau),
    % Appel récursif pour le joueur adverse
    minmax(NouveauPlateau, Profondeur, 'O', Alpha, Beta, _, Score),
    % Mettre à jour Beta
    NouvelBeta is min(Beta, Score),
    % Continuer l'exploration ou couper (élagage)
    (Alpha >= NouvelBeta ->
        % Élagage Alpha
        MeilleurScore = NouvelBeta,
        MeilleurCoup = Coup
    ; evaluer_min_suite(Plateau, Profondeur, Coups, Alpha, NouvelBeta, Coup, Score, MeilleurCoup, MeilleurScore)
    ).

evaluer_min(_, _, [], _, _, Coup, Score, Coup, Score).

evaluer_min_suite(Plateau, Profondeur, Coups, Alpha, Beta, CoupActuel, ScoreActuel, MeilleurCoup, MeilleurScore) :-
    evaluer_min(Plateau, Profondeur, Coups, Alpha, Beta, CoupSuivant, ScoreSuivant),
    (ScoreSuivant < ScoreActuel ->
        MeilleurCoup = CoupSuivant,
        MeilleurScore = ScoreSuivant
    ; MeilleurCoup = CoupActuel,
      MeilleurScore = ScoreActuel
    ).

% Évaluation de la position
evaluer_position(Plateau, Joueur, Score) :-
    % Vérifier la victoire
    (verifier_victoire(Plateau, 'O') -> Score = 1000
    ; verifier_victoire(Plateau, 'X') -> Score = -1000
    ; % Sinon, évaluer la position heuristiquement
      evaluer_heuristique(Plateau, Joueur, Score)
    ).

% Heuristique d'évaluation
evaluer_heuristique(Plateau, Joueur, Score) :-
    compter_alignements(Plateau, 'O', ScoreIA),
    compter_alignements(Plateau, 'X', ScoreHumain),
    Score is ScoreIA - ScoreHumain.

% Compte les alignements potentiels
compter_alignements(Plateau, Joueur, Score) :-
    % Compter les alignements horizontaux, verticaux et diagonaux
    compter_horizontaux(Plateau, Joueur, ScoreH),
    compter_verticaux(Plateau, Joueur, ScoreV),
    compter_diagonaux(Plateau, Joueur, ScoreD),
    Score is ScoreH + ScoreV + ScoreD.

% Prédicats auxiliaires pour vérifier les alignements
% (à implémenter selon les règles spécifiques du jeu)
compter_horizontaux(_, _, 0).
compter_verticaux(_, _, 0).
compter_diagonaux(_, _, 0).

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