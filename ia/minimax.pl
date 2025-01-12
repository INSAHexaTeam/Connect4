:- module(minimax, [choisir_colonne_minimax/2, simuler_coup/4]).
:- use_module('../gestion/joueurs', [joueur_peut_jouer/1, changer_joueur/2]).
:- use_module('../ia/heuristique'). % Import des fonctions d'évaluation

% Profondeur maximale pour la recherche Minimax
profondeur_max(4). % Ajustez la profondeur selon vos besoins

% Choix d'une colonne par l'IA utilisant Minimax
choisir_colonne_minimax(Plateau, Colonne) :-
    writeln("[DEBUG] Début de choisir_colonne_minimax."),
    profondeur_max(ProfondeurMax),
    mouvements_possibles(Plateau, Mouvements),
    writeln("[DEBUG] Colonnes jouables pour Minimax : "), writeln(Mouvements),
    (Mouvements = [] ->
        writeln("[ERREUR] Aucun mouvement possible."),
        fail
    ;
        minimax(Plateau, 'O', ProfondeurMax, -inf, inf, _, Colonne),
        format("[DEBUG] Minimax a choisi la colonne : ~w\n", [Colonne])
    ).

% Minimax : recherche récursive avec élagage alpha-bêta
minimax(Plateau, Joueur, 0, _, _, Score, _) :-
    evaluer_plateau(Plateau, Joueur, Score), % Évalue le plateau pour le joueur
    format("[DEBUG] Évaluation à profondeur 0. Score : ~w\n", [Score]),
    !.
minimax(Plateau, Joueur, Profondeur, Alpha, Beta, MeilleurScore, MeilleureColonne) :-
    Profondeur > 0,
    mouvements_possibles(Plateau, Mouvements),
    (Mouvements = [] -> % Aucun mouvement possible
        MeilleurScore = 0,
        MeilleureColonne = -1,
        writeln("[DEBUG] Aucun mouvement possible à cette profondeur.")
    ;
        explorer_mouvements(Mouvements, Plateau, Joueur, Profondeur, Alpha, Beta, -inf, MeilleurScore, MeilleureColonne)
    ).

% Exploration des mouvements possibles
explorer_mouvements([], _, _, _, _, _, MeilleurScore, MeilleurScore, _) :- !.
explorer_mouvements([Col|Cols], Plateau, Joueur, Profondeur, Alpha, Beta, ScoreCourant, MeilleurScore, MeilleureColonne) :-
    (simuler_coup(Plateau, Col, Joueur, NouveauPlateau) ->
        changer_joueur(Joueur, Adversaire),
        Profondeur1 is Profondeur - 1,
        minimax(NouveauPlateau, Adversaire, Profondeur1, Alpha, Beta, ScoreAdversaire, _),
        ScoreActuel is -ScoreAdversaire, % Inverse le score pour minimiser l'adversaire
        format("[DEBUG] Colonne ~w, Score obtenu : ~w\n", [Col, ScoreActuel]),
        (ScoreActuel > ScoreCourant ->
            NouveauScore = ScoreActuel,
            NouvelleMeilleureColonne = Col
        ;
            NouveauScore = ScoreCourant,
            NouvelleMeilleureColonne = MeilleureColonne
        ),
        % Élagage alpha-bêta
        (NouveauScore >= Beta ->
            MeilleurScore = NouveauScore,
            MeilleureColonne = NouvelleMeilleureColonne,
            writeln("[DEBUG] Coupure alpha-bêta."),
            !
        ;
            max(Alpha, NouveauScore, Alpha1),
            explorer_mouvements(Cols, Plateau, Joueur, Profondeur, Alpha1, Beta, NouveauScore, MeilleurScore, NouvelleMeilleureColonne)
        )
    ;
        % Si le coup échoue, passe au suivant
        writeln("[DEBUG] Simulation échouée pour la colonne : "), writeln(Col),
        explorer_mouvements(Cols, Plateau, Joueur, Profondeur, Alpha, Beta, ScoreCourant, MeilleurScore, MeilleureColonne)
    ).

% Génération des colonnes jouables
mouvements_possibles(Plateau, Mouvements) :-
    findall(Col, 
        (between(1, 7, Col), joueur_peut_jouer(Col)),
        Mouvements).

% Simulation d'un coup (ajout d'un pion dans une colonne)
simuler_coup(Plateau, Colonne, Joueur, NouveauPlateau) :-
    nth1(Colonne, Plateau, ListeColonne),
    length(ListeColonne, Taille),
    (Taille < 6 ->
        append(ListeColonne, [Joueur], NouvelleColonne),
        remplacer_colonne(Plateau, Colonne, NouvelleColonne, NouveauPlateau)
    ;
        fail % Colonne pleine
    ).

% Remplacement d'une colonne dans le plateau
remplacer_colonne([_|T], 1, NouvelleColonne, [NouvelleColonne|T]) :- !.
remplacer_colonne([H|T], N, NouvelleColonne, [H|R]) :-
    N > 1, N1 is N - 1,
    remplacer_colonne(T, N1, NouvelleColonne, R).

% Calcul du maximum entre deux valeurs
max(A, B, Max) :-
    (A >= B -> Max = A ; Max = B).
