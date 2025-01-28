% filepath: /Users/isalinefoissey/Connect4/test/test_minimax_vs_minimax_poids_colonnes.pl
:- consult('../gestion/joueurs').
:- consult('../gestion/victoire').
:- use_module('../ia/minimax', [simuler_coup/4, choisir_colonne_minimax/2]).
:- use_module('../ia/minimax_poids_colonnes', [simuler_coup_poids_colonnes/4, choisir_colonne_minimax_poids_colonnes/2]).

:- dynamic plateau_actuel/1.
:- dynamic victoires_ia_minimax_poids_colonnes/1.
:- dynamic coups_ia_minimax_poids_colonnes/1.

% Initialisation des compteurs de victoires et de coups
victoires_ia_minimax_poids_colonnes(0).
coups_ia_minimax_poids_colonnes([]).

% Définir plusieurs configurations de plateau NON VIDES
configurations_plateau_remplis_poids_colonnes([
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

% Fonction pour jouer un tour avec l'IA minimax poids colonnes
jouer_tour_ia_poids_colonnes(Joueur, TypeJoueur1, TypeJoueur2, NbCoups) :-
    plateau_actuel(Plateau),
    format("Plateau actuel: ~w\n", [Plateau]),  % Ajout d'une instruction de débogage
    (Joueur = 'X' -> TypeJoueur = TypeJoueur1 ; TypeJoueur = TypeJoueur2),
    format("Joueur: ~w, TypeJoueur: ~w\n", [Joueur, TypeJoueur]),  % Ajout d'une instruction de débogage
    demander_colonne(Joueur, Colonne, TypeJoueur),
    format("Colonne choisie: ~w\n", [Colonne]),  % Ajout d'une instruction de débogage
    (joueur_peut_jouer(Colonne) ->
        simuler_coup_poids_colonnes(Plateau, Colonne, Joueur, NouveauPlateau),
        format("Nouveau plateau après coup: ~w\n", [NouveauPlateau]),  % Ajout d'une instruction de débogage
        (verifier_victoire(NouveauPlateau, Joueur) ->
            retractall(plateau_actuel(_)),
            assert(plateau_actuel(NouveauPlateau)),
            TotalCoups is NbCoups + 1,
            afficher_plateau(NouveauPlateau),
            (Joueur = 'X' -> JoueurTexte = "X (IA Minimax)" ; JoueurTexte = "O (IA Minimax Poids Colonnes)"),
            format("Le joueur ~w a gagné en ~w coups!\n", [JoueurTexte, TotalCoups]),
            incrementer_compteur_victoires_poids_colonnes(TypeJoueur, TotalCoups),
            !
        ;
            changer_joueur(Joueur, ProchainJoueur),
            retractall(plateau_actuel(_)),
            assert(plateau_actuel(NouveauPlateau)),
            jouer_tour_ia_poids_colonnes(ProchainJoueur, TypeJoueur1, TypeJoueur2, NbCoups + 1)
        )
    ;
        jouer_tour_ia_poids_colonnes(Joueur, TypeJoueur1, TypeJoueur2, NbCoups)
    ).

% Fonction pour demander la colonne à jouer
demander_colonne(Joueur, Colonne, TypeJoueur) :-
    (TypeJoueur = ia_minimax ->
        choisir_colonne_minimax(Joueur, Colonne)
    ;
        choisir_colonne_minimax_poids_colonnes(Joueur, Colonne)
    ).

% Fonction pour vérifier si un joueur peut jouer dans une colonne
joueur_peut_jouer(Colonne) :-
    plateau_actuel(Plateau),
    nth1(Colonne, Plateau, Liste),
    length(Liste, L),
    L < 6.

% Fonction pour vérifier la victoire
verifier_victoire(Plateau, Joueur) :-
    victoire(Joueur, Plateau).

% Fonction pour changer de joueur
changer_joueur('X', 'O').
changer_joueur('O', 'X').

% Fonction pour afficher le plateau
afficher_plateau(Plateau) :-
    maplist(writeln, Plateau).

% Fonction pour incrémenter le compteur de victoires
incrementer_compteur_victoires_poids_colonnes(TypeJoueur, TotalCoups) :-
    victoires_ia_minimax_poids_colonnes(Victoires),
    NouveauVictoires is Victoires + 1,
    retractall(victoires_ia_minimax_poids_colonnes(_)),
    assert(victoires_ia_minimax_poids_colonnes(NouveauVictoires)),
    coups_ia_minimax_poids_colonnes(Coups),
    append(Coups, [TotalCoups], NouveauCoups),
    retractall(coups_ia_minimax_poids_colonnes(_)),
    assert(coups_ia_minimax_poids_colonnes(NouveauCoups)).

% Fonction pour tester les configurations de plateaux remplis
tester_configurations_remplis_poids_colonnes :-
    configurations_plateau_remplis_poids_colonnes(Configs),
    forall(member(Config, Configs), (
        retractall(plateau_actuel(_)),
        assert(plateau_actuel(Config)),
        jouer_une_partie_poids_colonnes(ia_minimax, ia_minimax_poids_colonnes)
    )).

% Fonction pour jouer une partie avec les deux IA
jouer_une_partie_poids_colonnes(TypeJoueur1, TypeJoueur2) :-
    jouer_tour_ia_poids_colonnes('X', TypeJoueur1, TypeJoueur2, 0).

% Fonction principale pour tester les performances
tester_performances_poids_colonnes :-
    writeln('Tests des IA sur plateaux NON VIDES :'),
    tester_configurations_remplis_poids_colonnes,
    victoires_ia_minimax_poids_colonnes(Victoires),
    coups_ia_minimax_poids_colonnes(Coups),
    format("Nombre de victoires de l'IA Minimax Poids Colonnes : ~w\n", [Victoires]),
    format("Liste des coups pour chaque victoire : ~w\n", [Coups]).