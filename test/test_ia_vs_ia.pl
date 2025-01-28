% filepath: /Users/isalinefoissey/Connect4/test/test_ia_vs_ia.pl
% Fichier : test_ia_vs_ia.pl

:- consult('../gestion/joueurs').
:- consult('../gestion/victoire').
:- consult('../ia/aleatoire').
:- use_module('../ia/minimax', [simuler_coup/4, choisir_colonne_minimax/2]).

:- dynamic plateau_actuel/1.
:- dynamic victoires_ia_aleatoire/1.
:- dynamic victoires_ia_minimax/1.
:- dynamic coups_ia_aleatoire/1.
:- dynamic coups_ia_minimax/1.

% Initialisation des compteurs de victoires et de coups
victoires_ia_aleatoire(0).
victoires_ia_minimax(0).
coups_ia_aleatoire([]).
coups_ia_minimax([]).

% Définir plusieurs configurations de plateau NON VIDES
configurations_plateau_remplis([
    [['X', 'X', 'X'], [], [], [], [], [], []],  % Victoire possible pour X
    [[], [], [], [], [], [], ['O', 'O', 'O']],  % Victoire possible pour O
    [[], [], [], ['O'], [], ['O'], ['O']],      % Plateau presque victoire pour O
    [[], [], [], ['X'], [], ['X'], ['X']],      % Plateau presque victoire pour O
    [['X'], ['O', 'X'], ['X', 'O', 'X'], ['O', 'O', 'O'], ['X'], ['O'], ['X']],  % Diagonale pour X
    [['O'], ['X', 'O'], ['X', 'O', 'O'], ['O', 'O', 'X'], ['X'], ['O'], ['X']],  % Diagonale pour X
    [['X', 'X'], [], [], [], [], [], []],        % Victoire presque pour X
    [['O', 'O'], [], [], [], [], [], []],       % Victoire presque pour O
    [['O'], ['X', 'O', 'X', 'O'], ['X', 'O', 'O'], ['O', 'O', 'X'], ['X'], ['O'], ['X']]
]).

% Définir des configurations de plateau VIDES
configurations_plateau_vides([
    [[], [], [], [], [], [], []]
]).

% Pour chaque configuration, on affiche le plateau, on joue,
% puis on affiche le vainqueur et le nombre de coups.
tester_configurations_remplis :-
    configurations_plateau_remplis(Configs),
    forall(
        member(Config, Configs),
        (
            retractall(plateau_actuel(_)),
            assert(plateau_actuel(Config)),
            afficher_plateau(Config),
            jouer_une_partie(ia_aleatoire, ia_minimax)
        )
    ).

% Idem pour les plateaux vides
tester_configurations_vides(N) :-
    configurations_plateau_vides(Configs),
    forall(
        member(Config, Configs),
        jouer_plusieurs_parties(Config, ia_aleatoire, ia_minimax, N)
    ).

% On regroupe tout dans tester_performances
tester_performances :-
    % Remise à zéro des compteurs
    retractall(victoires_ia_aleatoire(_)),
    assert(victoires_ia_aleatoire(0)),
    retractall(victoires_ia_minimax(_)),
    assert(victoires_ia_minimax(0)),
    retractall(coups_ia_aleatoire(_)),
    assert(coups_ia_aleatoire([])),
    retractall(coups_ia_minimax(_)),
    assert(coups_ia_minimax([])),

    % Tests sur plateaux NON VIDES, un par un
    writeln("Tests des IA sur plateaux NON VIDES :"),
    tester_configurations_remplis,
    % Affichage du bilan
    afficher_bilan_non_vides,

    % Tests sur plateaux VIDES
    writeln("\nTest des IA sur des plateaux vides :"),
    tester_configurations_vides(10),
    afficher_bilan_vides.

% On affiche le bilan des plateaux NON VIDES
afficher_bilan_non_vides :-
    victoires_ia_aleatoire(VA),
    victoires_ia_minimax(VM),
    % Récupération de la liste de coups enregistrée
    coups_ia_aleatoire(LCA),
    coups_ia_minimax(LCM),
    moyenne_coups(LCA, MCA),
    moyenne_coups(LCM, MCM),
    format("Nombre de victoires de l'IA aléatoire : ~w\n", [VA]),
    format("Nombre de victoires de l'IA Minimax : ~w\n", [VM]),
    format("Nombre moyen de coups avant victoire pour l'IA aléatoire X : ~2f\n", [MCA]),
    format("Nombre moyen de coups avant victoire pour l'IA Minimax 0 : ~2f\n", [MCM]),
    format("Fin du test sur plateaux non vides.\n", []),

    % On efface les compteurs avant de tester les plateaux vides
    retractall(victoires_ia_aleatoire(_)),
    assert(victoires_ia_aleatoire(0)),
    retractall(victoires_ia_minimax(_)),
    assert(victoires_ia_minimax(0)),
    retractall(coups_ia_aleatoire(_)),
    assert(coups_ia_aleatoire([])),
    retractall(coups_ia_minimax(_)),
    assert(coups_ia_minimax([])).


% Bilan final sur les plateaux VIDES
afficher_bilan_vides :-
    victoires_ia_aleatoire(VA),
    victoires_ia_minimax(VM),
    coups_ia_aleatoire(LCA),
    coups_ia_minimax(LCM),
    moyenne_coups(LCA, MCA),
    moyenne_coups(LCM, MCM),
    sum_list(LCA, TotalA), sum_list(LCM, TotalM),
    format("Nombre de test : 10\n", []),
    format("Nombre de victoires de l'IA aléatoire X : ~w\n", [VA]),
    format("Nombre de victoires de l'IA Minimax O : ~w\n", [VM]),
    format("Nombre moyen de coups avant victoire pour l'IA aléatoire X : ~2f\n", [MCA]),
    format("Nombre moyen de coups avant victoire pour l'IA Minimax O : ~2f\n", [MCM]),
    format("Nombre total de coups pour l'IA aléatoire X : ~w\n", [TotalA]),
    format("Nombre total de coups pour l'IA Minimax O : ~w\n", [TotalM]).
   
% Jouer plusieurs parties sur un même plateau
jouer_plusieurs_parties(_, _, _, 0) :- !.
jouer_plusieurs_parties(Plateau, IA1, IA2, N) :-
    retractall(plateau_actuel(_)),
    assert(plateau_actuel(Plateau)),
    %afficher_plateau(Plateau),
    jouer_une_partie(IA1, IA2),
    N1 is N - 1,
    jouer_plusieurs_parties(Plateau, IA1, IA2, N1).

% Jouer une partie complète IA1 vs IA2
jouer_une_partie(IA1, IA2) :-
    jouer_tour_ia('X', IA1, IA2, 0).

% Gérer un tour de jeu pour deux IA
jouer_tour_ia(Joueur, TypeJoueur1, TypeJoueur2, NbCoups) :-
    plateau_actuel(Plateau),
    (Joueur = 'X' -> TypeJoueur = TypeJoueur1 ; TypeJoueur = TypeJoueur2),
    demander_colonne(Joueur, Colonne, TypeJoueur),
    (joueur_peut_jouer(Colonne) ->
        simuler_coup(Plateau, Colonne, Joueur, NouveauPlateau),
        (verifier_victoire(NouveauPlateau, Joueur) ->
            retractall(plateau_actuel(_)),
            assert(plateau_actuel(NouveauPlateau)),
            TotalCoups is NbCoups + 1,
            afficher_plateau(NouveauPlateau),
            (Joueur = 'X' -> JoueurTexte = "X (IA aléatoire)" ; JoueurTexte = "O (IA minimax)"),
            format("Le joueur ~w a gagné en ~w coups!\n", [JoueurTexte, TotalCoups]),
            incrementer_compteur_victoires(TypeJoueur, TotalCoups),
            !
        ;
            changer_joueur(Joueur, ProchainJoueur),
            retractall(plateau_actuel(_)),
            assert(plateau_actuel(NouveauPlateau)),
            jouer_tour_ia(ProchainJoueur, TypeJoueur1, TypeJoueur2, NbCoups + 1)
        )
    ;
        jouer_tour_ia(Joueur, TypeJoueur1, TypeJoueur2, NbCoups)
    ).

% Incrémenter et stocker les données de victoire
incrementer_compteur_victoires(TypeJoueur, NbCoups) :-
    ( TypeJoueur = ia_aleatoire ->
        victoires_ia_aleatoire(VA),
        VA2 is VA + 1,
        retractall(victoires_ia_aleatoire(_)),
        assert(victoires_ia_aleatoire(VA2)),
        coups_ia_aleatoire(CA),
        CA2 = [NbCoups|CA],
        retractall(coups_ia_aleatoire(_)),
        assert(coups_ia_aleatoire(CA2))
    ; TypeJoueur = ia_minimax ->
        victoires_ia_minimax(VM),
        VM2 is VM + 1,
        retractall(victoires_ia_minimax(_)),
        assert(victoires_ia_minimax(VM2)),
        coups_ia_minimax(CM),
        CM2 = [NbCoups|CM],
        retractall(coups_ia_minimax(_)),
        assert(coups_ia_minimax(CM2))
    ).

% Calculer la moyenne de la liste de coups
moyenne_coups([], 0) :- !.
moyenne_coups(Liste, Moyenne) :-
    sum_list(Liste, Somme),
    length(Liste, N),
    Moyenne is Somme / N.