:- consult('../gestion/joueurs').
:- consult('../gestion/victoire').
:- use_module('../ia/minimax_defensive_ameliore', [simuler_coup_defensive_ameliore/4, choisir_colonne_minimax_defensive_ameliore/2]).
:- use_module('../ia/minimax_defensive', [simuler_coup_defensive/4, choisir_colonne_minimax_defensive/2]).

:- dynamic plateau_actuel/1.
:- dynamic victoires_ia_minimax_defensive_ameliore/1.
:- dynamic victoires_ia_minimax_defensive/1.
:- dynamic coups_ia_minimax_defensive_ameliore/1.
:- dynamic coups_ia_minimax_defensive/1.

% Initialisation des compteurs de victoires et de coups
:- retractall(victoires_ia_minimax_defensive_ameliore(_)), assert(victoires_ia_minimax_defensive_ameliore(0)).
:- retractall(victoires_ia_minimax_defensive(_)), assert(victoires_ia_minimax_defensive(0)).
:- retractall(coups_ia_minimax_defensive_ameliore(_)), assert(coups_ia_minimax_defensive_ameliore([])).
:- retractall(coups_ia_minimax_defensive(_)), assert(coups_ia_minimax_defensive([])).

% Définir plusieurs configurations de plateau NON VIDES
configurations_plateau_remplis_def2([
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
configurations_plateau_vides_def2([
    [[], [], [], [], [], [], []]
]).

% Pour chaque configuration, on affiche le plateau, on joue,
% puis on affiche le vainqueur et le nombre de coups.
tester_configurations_remplis_def2 :-
    configurations_plateau_remplis_def2(Configs),
    forall(
        member(Config, Configs),
        (
            retractall(plateau_actuel(_)),
            assert(plateau_actuel(Config)),
            writeln(""),
            writeln("Nouveau plateau :"),
            afficher_plateau(Config),
            writeln("Le joueur X (IA minimax_defensive_ameliore) commence."),
            jouer_une_partie_def2(ia_minimax_defensive_ameliore, ia_minimax_defensive, 'X'),
            retractall(plateau_actuel(_)),
            assert(plateau_actuel(Config)),
            writeln("Le joueur O (IA minimax_defensive) commence."),
            jouer_une_partie_def2(ia_minimax_defensive, ia_minimax_defensive_ameliore, 'O')
        )
    ).

% Idem pour les plateaux vides
tester_configurations_vides_def2(N) :-
    configurations_plateau_vides_def2(Configs),
    forall(
        member(Config, Configs),
        jouer_plusieurs_parties_def2(Config, ia_minimax_defensive_ameliore, ia_minimax_defensive, N)
    ).

% On regroupe tout dans tester_performances
tester_performances_defensive_defensive_ameliore :-
    % Remise à zéro des compteurs
    retractall(victoires_ia_minimax_defensive_ameliore(_)),
    assert(victoires_ia_minimax_defensive_ameliore(0)),
    retractall(victoires_ia_minimax_defensive(_)),
    assert(victoires_ia_minimax_defensive(0)),
    retractall(coups_ia_minimax_defensive_ameliore(_)),
    assert(coups_ia_minimax_defensive_ameliore([])),
    retractall(coups_ia_minimax_defensive(_)),
    assert(coups_ia_minimax_defensive([])),

    % Tests sur plateaux NON VIDES, un par un
    writeln("Tests des IA sur plateaux NON VIDES :"),
    tester_configurations_remplis_def2,
    % Affichage du bilan
    afficher_bilan_non_vides_def2,

    % Tests sur plateaux VIDES
    writeln("\nTest des IA sur des plateaux vides :"),
    tester_configurations_vides_def2(10),
    afficher_bilan_vides_def2.

% On affiche le bilan des plateaux NON VIDES
afficher_bilan_non_vides_def2 :-
    victoires_ia_minimax_defensive_ameliore(VM),
    victoires_ia_minimax_defensive(VMD),
    coups_ia_minimax_defensive_ameliore(LCM),
    coups_ia_minimax_defensive(LCMD),
    moyenne_coups_def2(LCM, MCM),
    moyenne_coups_def2(LCMD, MCMD),
    format("Nombre de victoires de l'IA minimax_defensive_ameliore : ~w\n", [VM]),
    format("Nombre de victoires de l'IA minimax_defensive : ~w\n", [VMD]),
    format("Nombre moyen de coups avant victoire pour l'IA minimax_defensive_ameliore : ~2f\n", [MCM]),
    format("Nombre moyen de coups avant victoire pour l'IA minimax_defensive : ~2f\n", [MCMD]),
    format("Fin du test sur plateaux non vides.\n", []),

    % On efface les compteurs avant de tester les plateaux vides
    retractall(victoires_ia_minimax_defensive_ameliore(_)),
    assert(victoires_ia_minimax_defensive_ameliore(0)),
    retractall(victoires_ia_minimax_defensive(_)),
    assert(victoires_ia_minimax_defensive(0)),
    retractall(coups_ia_minimax_defensive_ameliore(_)),
    assert(coups_ia_minimax_defensive_ameliore([])),
    retractall(coups_ia_minimax_defensive(_)),
    assert(coups_ia_minimax_defensive([])).

% Bilan final sur les plateaux VIDES
afficher_bilan_vides_def2 :-
    victoires_ia_minimax_defensive_ameliore(VM),
    victoires_ia_minimax_defensive(VMD),
    coups_ia_minimax_defensive_ameliore(LCM),
    coups_ia_minimax_defensive(LCMD),
    moyenne_coups_def2(LCM, MCM),
    moyenne_coups_def2(LCMD, MCMD),
    sum_list(LCM, TotalM), sum_list(LCMD, TotalMD),
    format("Nombre de test : 10\n", []),
    format("Nombre de victoires de l'IA minimax_defensive_ameliore : ~w\n", [VM]),
    format("Nombre de victoires de l'IA minimax_defensive : ~w\n", [VMD]),
    format("Nombre moyen de coups avant victoire pour l'IA minimax_defensive_ameliore : ~2f\n", [MCM]),
    format("Nombre moyen de coups avant victoire pour l'IA minimax_defensive : ~2f\n", [MCMD]),
    format("Nombre total de coups pour l'IA minimax_defensive_ameliore : ~w\n", [TotalM]),
    format("Nombre total de coups pour l'IA minimax_defensive : ~w\n", [TotalMD]).

% Jouer plusieurs parties sur un même plateau
jouer_plusieurs_parties_def2(_, _, _, 0) :- !.
jouer_plusieurs_parties_def2(Plateau, IA1, IA2, N) :-
    retractall(plateau_actuel(_)),
    assert(plateau_actuel(Plateau)),
    (N mod 2 =:= 0 -> jouer_une_partie_def2(IA1, IA2, 'X') ; jouer_une_partie_def2(IA1, IA2, 'O')),
    N1 is N - 1,
    jouer_plusieurs_parties_def2(Plateau, IA1, IA2, N1).

% Jouer une partie complète IA1 vs IA2
jouer_une_partie_def2(IA1, IA2, PremierJoueur) :-
    jouer_tour_ia_def2(PremierJoueur, IA1, IA2, 0).

% Gérer un tour de jeu pour deux IA
jouer_tour_ia_def2(Joueur, TypeJoueur1, TypeJoueur2, NbCoups) :-
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
            (Joueur = 'X' -> JoueurTexte = "X (IA minimax_defensive_ameliore)" ; JoueurTexte = "O (IA minimax_defensive)"),
            format("Le joueur ~w a gagné en ~w coups!\n", [JoueurTexte, TotalCoups]),
            incrementer_compteur_victoires_def2(TypeJoueur, TotalCoups),
            !
        ;
            changer_joueur(Joueur, ProchainJoueur),
            retractall(plateau_actuel(_)),
            assert(plateau_actuel(NouveauPlateau)),
            jouer_tour_ia_def2(ProchainJoueur, TypeJoueur1, TypeJoueur2, NbCoups + 1)
        )
    ;
        jouer_tour_ia_def2(Joueur, TypeJoueur1, TypeJoueur2, NbCoups)
    ).

% Incrémenter et stocker les données de victoire
incrementer_compteur_victoires_def2(TypeJoueur, NbCoups) :-
    ( TypeJoueur = ia_minimax_defensive_ameliore ->
        victoires_ia_minimax_defensive_ameliore(VM),
        VM2 is VM + 1,
        retractall(victoires_ia_minimax_defensive_ameliore(_)),
        assert(victoires_ia_minimax_defensive_ameliore(VM2)),
        coups_ia_minimax_defensive_ameliore(CM),
        CM2 = [NbCoups|CM],
        retractall(coups_ia_minimax_defensive_ameliore(_)),
        assert(coups_ia_minimax_defensive_ameliore(CM2))
    ; TypeJoueur = ia_minimax_defensive ->
        victoires_ia_minimax_defensive(VMD),
        VMD2 is VMD + 1,
        retractall(victoires_ia_minimax_defensive(_)),
        assert(victoires_ia_minimax_defensive(VMD2)),
        coups_ia_minimax_defensive(CMD),
        CMD2 = [NbCoups|CMD],
        retractall(coups_ia_minimax_defensive(_)),
        assert(coups_ia_minimax_defensive(CMD2))
    ).

% Calculer la moyenne de la liste de coups
moyenne_coups_def2([], 0) :- !.
moyenne_coups_def2(Liste, Moyenne) :-
    sum_list(Liste, Somme),
    length(Liste, N),
    Moyenne is Somme / N.