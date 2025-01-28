:- consult('../gestion/joueurs').
:- consult('../gestion/victoire').
:- use_module('../ia/minimax', [simuler_coup/4, choisir_colonne_minimax/2]).
:- use_module('../ia/minimax_poids_colonnes', [simuler_coup_poids_colonnes/4, choisir_colonne_minimax_poids_colonnes/2]).

:- dynamic plateau_actuel/1.
:- dynamic victoires_ia_minimax/1.
:- dynamic victoires_ia_minimax_poids_colonnes/1.
:- dynamic coups_ia_minimax/1.
:- dynamic coups_ia_minimax_poids_colonnes/1.

% Initialisation des compteurs de victoires et de coups
:- retractall(victoires_ia_minimax(_)), assert(victoires_ia_minimax(0)).
:- retractall(victoires_ia_minimax_poids_colonnes(_)), assert(victoires_ia_minimax_poids_colonnes(0)).
:- retractall(coups_ia_minimax(_)), assert(coups_ia_minimax([])).
:- retractall(coups_ia_minimax_poids_colonnes(_)), assert(coups_ia_minimax_poids_colonnes([])).

% Définir plusieurs configurations de plateau NON VIDES
configurations_plateau_remplis_poids_colonnes([
    [['X', 'X', 'X'], [], [], [], [], [], []],  % Victoire possible pour X
    [[], [], [], [], [], [], ['O', 'O', 'O']],  % Victoire possible pour O
    [[], [], ['X'], ['X'], [], ['X'], ['X']],      % Plateau presque victoire pour O
    [[], [], [], ['O'], [], ['O'], ['O']],      % Plateau presque victoire pour O
    [['X'], ['O', 'X'], ['X', 'O', 'X'], ['O', 'O', 'O'], ['X'], ['O'], ['X']],  % Diagonale pour X
    [['O'], ['X', 'O'], ['X', 'O', 'O'], ['O', 'O', 'X'], ['X'], ['O'], ['X']],  % Diagonale pour O
    [['X', 'X'], [], [], [], [], [], []],        % Victoire presque pour X
    [[], ['O', 'O'], [], [], [], [], []]
]).

% Définir des configurations de plateau VIDES
configurations_plateau_vides_poids_colonnes([
    [[], [], [], [], [], [], []]
]).

% Pour chaque configuration, on affiche le plateau, on joue,
% puis on affiche le vainqueur et le nombre de coups.
tester_configurations_remplis_poids_colonnes :-
    configurations_plateau_remplis_poids_colonnes(Configs),
    forall(
        member(Config, Configs),
        (
            retractall(plateau_actuel(_)),
            assert(plateau_actuel(Config)),
            writeln(""),
            writeln("Nouveau plateau :"),
            afficher_plateau(Config),
            writeln("Le joueur X (IA minimax_poids_colonnes) commence."),
            jouer_une_partie_poids_colonnes(ia_minimax_poids_colonnes, ia_minimax, 'X'),
            retractall(plateau_actuel(_)),
            assert(plateau_actuel(Config)),
            writeln("Le joueur O (IA minimax_defensive) commence."),
            jouer_une_partie_poids_colonnes(ia_minimax, ia_minimax_poids_colonnes, 'O')
        )
    ).

% Idem pour les plateaux vides
tester_configurations_vides_poids_colonnes(N) :-
    configurations_plateau_vides_poids_colonnes(Configs),
    forall(
        member(Config, Configs),
        jouer_plusieurs_parties_poids_colonnes(Config, ia_minimax, ia_minimax_poids_colonnes, N)
    ).

% On regroupe tout dans tester_performances
tester_performances_poids_colonnes :-
    % Remise à zéro des compteurs
    retractall(victoires_ia_minimax(_)),
    assert(victoires_ia_minimax(0)),
    retractall(victoires_ia_minimax_poids_colonnes(_)),
    assert(victoires_ia_minimax_poids_colonnes(0)),
    retractall(coups_ia_minimax(_)),
    assert(coups_ia_minimax([])),
    retractall(coups_ia_minimax_poids_colonnes(_)),
    assert(coups_ia_minimax_poids_colonnes([])),

    % Tests sur plateaux NON VIDES, un par un
    writeln("Tests des IA sur plateaux NON VIDES :"),
    tester_configurations_remplis_poids_colonnes,
    % Affichage du bilan
    afficher_bilan_non_vides_poids_colonnes,

    % Tests sur plateaux VIDES
    writeln("\nTest des IA sur des plateaux vides :"),
    tester_configurations_vides_poids_colonnes(10),
    afficher_bilan_vides_poids_colonnes.

% On affiche le bilan des plateaux NON VIDES
afficher_bilan_non_vides_poids_colonnes :-
    victoires_ia_minimax(VM),
    victoires_ia_minimax_poids_colonnes(VMD),
    coups_ia_minimax(LCM),
    coups_ia_minimax_poids_colonnes(LCMD),
    moyenne_coups_poids_colonnes(LCM, MCM),
    moyenne_coups_poids_colonnes(LCMD, MCMD),
    format("Nombre de victoires de l'IA Minimax : ~w\n", [VM]),
    format("Nombre de victoires de l'IA Minimax poids_colonnes : ~w\n", [VMD]),
    format("Nombre moyen de coups avant victoire pour l'IA Minimax : ~2f\n", [MCM]),
    format("Nombre moyen de coups avant victoire pour l'IA Minimax poids_colonnes : ~2f\n", [MCMD]),
    format("Fin du test sur plateaux non vides.\n", []),

    % On efface les compteurs avant de tester les plateaux vides
    retractall(victoires_ia_minimax(_)),
    assert(victoires_ia_minimax(0)),
    retractall(victoires_ia_minimax_poids_colonnes(_)),
    assert(victoires_ia_minimax_poids_colonnes(0)),
    retractall(coups_ia_minimax(_)),
    assert(coups_ia_minimax([])),
    retractall(coups_ia_minimax_poids_colonnes(_)),
    assert(coups_ia_minimax_poids_colonnes([])).

% Bilan final sur les plateaux VIDES
afficher_bilan_vides_poids_colonnes :-
    victoires_ia_minimax(VM),
    victoires_ia_minimax_poids_colonnes(VMD),
    coups_ia_minimax(LCM),
    coups_ia_minimax_poids_colonnes(LCMD),
    moyenne_coups_poids_colonnes(LCM, MCM),
    moyenne_coups_poids_colonnes(LCMD, MCMD),
    sum_list(LCM, TotalM), sum_list(LCMD, TotalMD),
    format("Nombre de test : 10\n", []),
    format("Nombre de victoires de l'IA Minimax : ~w\n", [VM]),
    format("Nombre de victoires de l'IA Minimax poids_colonnes : ~w\n", [VMD]),
    format("Nombre moyen de coups avant victoire pour l'IA Minimax : ~2f\n", [MCM]),
    format("Nombre moyen de coups avant victoire pour l'IA Minimax poids_colonnes : ~2f\n", [MCMD]),
    format("Nombre total de coups pour l'IA Minimax : ~w\n", [TotalM]),
    format("Nombre total de coups pour l'IA Minimax poids_colonnes : ~w\n", [TotalMD]).

% Jouer plusieurs parties sur un même plateau
jouer_plusieurs_parties_poids_colonnes(_, _, _, 0) :- !.
jouer_plusieurs_parties_poids_colonnes(Plateau, IA1, IA2, N) :-
    retractall(plateau_actuel(_)),
    assert(plateau_actuel(Plateau)),
    (N mod 2 =:= 0 -> jouer_une_partie_poids_colonnes(IA1, IA2, 'X') ; jouer_une_partie_poids_colonnes(IA1, IA2, 'O')),
    N1 is N - 1,
    jouer_plusieurs_parties_poids_colonnes(Plateau, IA1, IA2, N1).

% Jouer une partie complète IA1 vs IA2
jouer_une_partie_poids_colonnes(IA1, IA2, PremierJoueur) :-
    jouer_tour_ia_poids_colonnes(PremierJoueur, IA1, IA2, 0).

% Gérer un tour de jeu pour deux IA
jouer_tour_ia_poids_colonnes(Joueur, TypeJoueur1, TypeJoueur2, NbCoups) :-
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
            (Joueur = 'X' -> JoueurTexte = "X (IA Minimax)" ; JoueurTexte = "O (IA Minimax poids_colonnes)"),
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

% Incrémenter et stocker les données de victoire
incrementer_compteur_victoires_poids_colonnes(TypeJoueur, NbCoups) :-
    ( TypeJoueur = ia_minimax ->
        victoires_ia_minimax(VM),
        VM2 is VM + 1,
        retractall(victoires_ia_minimax(_)),
        assert(victoires_ia_minimax(VM2)),
        coups_ia_minimax(CM),
        CM2 = [NbCoups|CM],
        retractall(coups_ia_minimax(_)),
        assert(coups_ia_minimax(CM2))
    ; TypeJoueur = ia_minimax_poids_colonnes ->
        victoires_ia_minimax_poids_colonnes(VMD),
        VMD2 is VMD + 1,
        retractall(victoires_ia_minimax_poids_colonnes(_)),
        assert(victoires_ia_minimax_poids_colonnes(VMD2)),
        coups_ia_minimax_poids_colonnes(CMD),
        CMD2 = [NbCoups|CMD],
        retractall(coups_ia_minimax_poids_colonnes(_)),
        assert(coups_ia_minimax_poids_colonnes(CMD2))
    ).

% Calculer la moyenne de la liste de coups
moyenne_coups_poids_colonnes([], 0) :- !.
moyenne_coups_poids_colonnes(Liste, Moyenne) :-
    sum_list(Liste, Somme),
    length(Liste, N),
    Moyenne is Somme / N.