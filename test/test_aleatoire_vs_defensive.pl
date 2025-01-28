:- consult('../gestion/joueurs').
:- consult('../gestion/victoire').
:- consult('../ia/aleatoire').
:- consult('../ia/minimax_defensive').
:- use_module('../ia/minimax', [simuler_coup/4, choisir_colonne_minimax/2]).

:- dynamic plateau_actuel/1.
:- dynamic victoires_ia_aleatoire_def/1.
:- dynamic victoires_ia_defensive/1.
:- dynamic coups_ia_aleatoire_def/1.
:- dynamic coups_ia_defensive/1.

% Initialisation des compteurs de victoires et de coups
victoires_ia_aleatoire_def(0).
victoires_ia_defensive(0).
coups_ia_aleatoire_def([]).
coups_ia_defensive([]).

% Définir plusieurs configurations de plateau NON VIDES
configurations_plateau_remplis_def([
    [['X', 'X', 'X'], [], [], [], [], [], []],  % Victoire possible pour X
    [[], [], [], [], [], [], ['O', 'O', 'O']],  % Victoire possible pour O
    [[], [], [], ['O'], [], ['O'], ['O']],      % Plateau presque victoire pour O
    [[], [], [], ['X'], [], ['X'], ['X']],      % Plateau presque victoire pour O
    [['X'], ['O', 'X'], ['X', 'O', 'X'], ['O', 'O', 'O'], ['X'], ['O'], ['X']],  % Diagonale pour X
    [['O'], ['X', 'O'], ['X', 'O', 'O'], ['O', 'O', 'X'], ['X'], ['O'], ['X']],  % Diagonale pour X
    [['X', 'X'], [], [], [], [], [], []],        % Victoire presque pour X
    [[], ['O', 'O'], [], [], [], [], []]
]).

% Définir des configurations de plateau VIDES
configurations_plateau_vides_def([
    [[], [], [], [], [], [], []]
]).

% Pour chaque configuration, on affiche le plateau, on joue,
% puis on affiche le vainqueur et le nombre de coups.
tester_configurations_remplis_def :-
    configurations_plateau_remplis_def(Configs),
    forall(
        member(Config, Configs),
        (
            retractall(plateau_actuel(_)),
            assert(plateau_actuel(Config)),
            writeln(""),
            writeln("Nouveau plateau :"),
            afficher_plateau(Config),
            writeln("Le joueur X (IA aléatoire) commence."),
            jouer_une_partie_def(ia_aleatoire, ia_minimax_defensive),
            retractall(plateau_actuel(_)),
            assert(plateau_actuel(Config)),
            writeln("Le joueur O (IA minimax_defensive) commence."),
            jouer_une_partie_def(ia_minimax_defensive, ia_aleatoire)
        )
    ).

% Idem pour les plateaux vides
tester_configurations_vides_def(N) :-
    configurations_plateau_vides_def(Configs),
    forall(
        member(Config, Configs),
        jouer_plusieurs_parties_def(Config, ia_aleatoire, ia_minimax_defensive, N)
    ).

% On regroupe tout dans tester_performances
tester_aleatoire_vs_defensive :-
    % Remise à zéro des compteurs
    retractall(victoires_ia_aleatoire_def(_)),
    assert(victoires_ia_aleatoire_def(0)),
    retractall(victoires_ia_defensive(_)),
    assert(victoires_ia_defensive(0)),
    retractall(coups_ia_aleatoire_def(_)),
    assert(coups_ia_aleatoire_def([])),
    retractall(coups_ia_defensive(_)),
    assert(coups_ia_defensive([])),

    % Tests sur plateaux NON VIDES, un par un
    writeln("Tests des IA sur plateaux NON VIDES :"),
    tester_configurations_remplis_def,
    % Affichage du bilan
    afficher_bilan_non_vides_def,

    % Tests sur plateaux VIDES
    writeln("\nTest des IA sur des plateaux vides :"),
    tester_configurations_vides_def(10),
    afficher_bilan_vides_def.

% On affiche le bilan des plateaux NON VIDES
afficher_bilan_non_vides_def :-
    victoires_ia_aleatoire_def(VA),
    victoires_ia_defensive(VD),
    % Récupération de la liste de coups enregistrée
    coups_ia_aleatoire_def(LCA),
    coups_ia_defensive(LCD),
    moyenne_coups(LCA, MCA),
    moyenne_coups(LCD, MCD),
    format("Nombre de victoires de l'IA aléatoire : ~w\n", [VA]),
    format("Nombre de victoires de l'IA Defensive : ~w\n", [VD]),
    format("Nombre moyen de coups avant victoire pour l'IA aléatoire X : ~2f\n", [MCA]),
    format("Nombre moyen de coups avant victoire pour l'IA Defensive 0 : ~2f\n", [MCD]),
    format("Fin du test sur plateaux non vides.\n", []),

    % On efface les compteurs avant de tester les plateaux vides
    retractall(victoires_ia_aleatoire_def(_)),
    assert(victoires_ia_aleatoire_def(0)),
    retractall(victoires_ia_defensive(_)),
    assert(victoires_ia_defensive(0)),
    retractall(coups_ia_aleatoire_def(_)),
    assert(coups_ia_aleatoire_def([])),
    retractall(coups_ia_defensive(_)),
    assert(coups_ia_defensive([])).


% Bilan final sur les plateaux VIDES
afficher_bilan_vides_def :-
    victoires_ia_aleatoire_def(VA),
    victoires_ia_defensive(VD),
    coups_ia_aleatoire_def(LCA),
    coups_ia_defensive(LCD),
    moyenne_coups(LCA, MCA),
    moyenne_coups(LCD, MCD),
    sum_list(LCA, TotalA), sum_list(LCD, TotalD),
    format("Nombre de test : 10\n", []),
    format("Nombre de victoires de l'IA aléatoire X : ~w\n", [VA]),
    format("Nombre de victoires de l'IA Defensive O : ~w\n", [VD]),
    format("Nombre moyen de coups avant victoire pour l'IA aléatoire X : ~2f\n", [MCA]),
    format("Nombre moyen de coups avant victoire pour l'IA Defensive O : ~2f\n", [MCD]),
    format("Nombre total de coups pour l'IA aléatoire X : ~w\n", [TotalA]),
    format("Nombre total de coups pour l'IA Defensive O : ~w\n", [TotalD]).
   
% Jouer plusieurs parties sur un même plateau
jouer_plusieurs_parties_def(_, _, _, 0) :- !.
jouer_plusieurs_parties_def(Plateau, IA1, IA2, N) :-
    retractall(plateau_actuel(_)),
    assert(plateau_actuel(Plateau)),
    %afficher_plateau(Plateau),
    jouer_une_partie_def(IA1, IA2),
    N1 is N - 1,
    jouer_plusieurs_parties_def(Plateau, IA1, IA2, N1).

% Jouer une partie complète IA1 vs IA2
jouer_une_partie_def(IA1, IA2) :-
    jouer_tour_ia_def('X', IA1, IA2, 0).

% Gérer un tour de jeu pour deux IA
jouer_tour_ia_def(Joueur, TypeJoueur1, TypeJoueur2, NbCoups) :-
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
            (Joueur = 'X' -> JoueurTexte = "X (IA aléatoire)" ; JoueurTexte = "O (IA Defensive)"),
            format("Le joueur ~w a gagné en ~w coups!\n", [JoueurTexte, TotalCoups]),
            incrementer_compteur_victoires_def(TypeJoueur, TotalCoups),
            !
        ;
            changer_joueur(Joueur, ProchainJoueur),
            retractall(plateau_actuel(_)),
            assert(plateau_actuel(NouveauPlateau)),
            jouer_tour_ia_def(ProchainJoueur, TypeJoueur1, TypeJoueur2, NbCoups + 1)
        )
    ;
        jouer_tour_ia_def(Joueur, TypeJoueur1, TypeJoueur2, NbCoups)
    ).

% Incrémenter et stocker les données de victoire
incrementer_compteur_victoires_def(TypeJoueur, NbCoups) :-
    ( TypeJoueur = ia_aleatoire ->
        victoires_ia_aleatoire_def(VA),
        VA2 is VA + 1,
        retractall(victoires_ia_aleatoire_def(_)),
        assert(victoires_ia_aleatoire_def(VA2)),
        coups_ia_aleatoire_def(CA),
        CA2 = [NbCoups|CA],
        retractall(coups_ia_aleatoire_def(_)),
        assert(coups_ia_aleatoire_def(CA2))
    ; TypeJoueur = ia_minimax_defensive ->
        victoires_ia_defensive(VD),
        VD2 is VD + 1,
        retractall(victoires_ia_defensive(_)),
        assert(victoires_ia_defensive(VD2)),
        coups_ia_defensive(CD),
        CD2 = [NbCoups|CD],
        retractall(coups_ia_defensive(_)),
        assert(coups_ia_defensive(CD2))
    ).

% Calculer la moyenne de la liste de coups
moyenne_coups([], 0) :- !.
moyenne_coups(Liste, Moyenne) :-
    sum_list(Liste, Somme),
    length(Liste, N),
    Moyenne is Somme / N.