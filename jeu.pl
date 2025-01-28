% Fichier principal : jeu.pl
:- use_module('gestion/affichage.pl').
:- ensure_loaded('gestion/joueurs.pl').
:- ensure_loaded('gestion/victoire.pl').
:- ensure_loaded('ia/aleatoire').
:- use_module('ia/minimax', [simuler_coup/4]).
:- ensure_loaded('test/test_aleatoire_vs_minimax.pl').  % Charger les tests de performance
:- ensure_loaded('test/test_minimax_vs_defensive.pl').  % Charger les tests de performance
:- ensure_loaded('test/test_minimax_vs_minimax_poids_colonnes.pl').  % Charger les tests de performance
:- ensure_loaded('test/test_defensive_vs_poid_colonnes.pl').  % Charger les tests de performance
:- ensure_loaded('test/test_aleatoire_vs_defensive.pl').  % Charger les tests de performance
:- ensure_loaded('test/test_minimax_vs_defensive_ameliore.pl').  % Charger les tests de performance
:- ensure_loaded('test/test_defensive_vs_defensive_ameliore.pl').  % Charger les tests de performance

% Déclaration des prédicats discontigus
:- discontiguous jouer/0.
:- discontiguous choisir_mode_jeu/0.
:- discontiguous jouer_tour/3.

% Déclaration pour manipuler dynamiquement le plateau
:- dynamic plateau_actuel/1.

% Plateau initial vide
plateau_vide([[], [], [], [], [], [], []]).

% Démarrer le jeu
jouer :-
    retractall(plateau_actuel(_)),  % Supprime tout plateau existant
    plateau_vide(Plateau),
    assert(plateau_actuel(Plateau)),  % Initialise le plateau
    choisir_mode_jeu.

% Choisir le mode de jeu
choisir_mode_jeu :-
    writeln("Choisissez le mode de jeu :"),
    writeln("1. Joueur vs Joueur"),
    writeln("2. Joueur vs IA (aleatoire)"),
    writeln("3. Joueur vs IA (Minimax)"),
    writeln("4. Joueur vs IA (Minimax) - Poids des colonnes"),
    writeln("5. Joueur vs IA (Minimax) - Defensive"),
    writeln("6. Joueur vs IA (Minimax) - Defensive amélioré"),
    writeln("7. Tester les performances des IA"),
    writeln("8. Tester Minimax vs Defensive"),
    writeln("9. Tester Minimax vs Minimax - Poids des colonnes"),
    writeln("10. Tester Minimax Defensive vs Minimax - Poids des colonnes"),
    writeln("11. Tester Aléatoire vs Defensive"),
    writeln("12. Tester Minimax vs Defensive Ameliore"),
    writeln("13. Tester Defensive vs Defensive Ameliore"),
    writeln("14. Quitter"),
    catch(read(Mode), _, Mode = invalide),
    (integer(Mode), between(1, 13, Mode) ->
        (Mode = 1 ->
            jouer_tour('X', humain, humain)
        ; Mode = 2 ->
            jouer_tour('X', humain, ia_aleatoire)
        ; Mode = 3 ->
            jouer_tour('X', humain, ia_minimax)
        ; Mode = 4 ->
            jouer_tour('X', humain, ia_minimax_poids_colonnes)
        ; Mode = 5 ->
            jouer_tour('X', humain, ia_minimax_defensive)
        ; Mode = 6 -> 
            jouer_tour('X', humain, ia_minimax_defensive_ameliore)
        ; Mode = 7 ->
            tester_performances
        ; Mode = 8 ->
            tester_performances_defensive
        ; Mode = 9 ->
            tester_performances_poids_colonnes
        ; Mode = 10 ->
            tester_performances_defensive_poids_colonnes
        ; Mode = 11 ->
            tester_aleatoire_vs_defensive
        ; Mode = 12 ->
            tester_performances_defensive_ameliore
        ; Mode = 13 ->
            tester_performances_defensive_defensive_ameliore
        ; Mode = 14 ->
            writeln("Au revoir !"), halt
        )
    ;
        writeln("Mode invalide, recommencez."),
        choisir_mode_jeu
    ).

% Gérer un tour de jeu
jouer_tour(Joueur, TypeJoueur1, TypeJoueur2) :-
    plateau_actuel(Plateau),  % Récupère le plateau actuel
    afficher_plateau(Plateau),
    (Joueur = 'X' -> TypeJoueur = TypeJoueur1 ; TypeJoueur = TypeJoueur2),
    demander_colonne(Joueur, Colonne, TypeJoueur),
    (joueur_peut_jouer(Colonne) ->  % Vérifie si la colonne est jouable
        simuler_coup(Plateau, Colonne, Joueur, NouveauPlateau),
        (verifier_victoire(NouveauPlateau, Joueur) ->
            retractall(plateau_actuel(_)),
            assert(plateau_actuel(NouveauPlateau)),
            afficher_plateau(NouveauPlateau),
            format("Le joueur ~w a gagné !\n", [Joueur]),
            halt  % Fin de la partie
        ;
            changer_joueur(Joueur, ProchainJoueur),
            retractall(plateau_actuel(_)),
            assert(plateau_actuel(NouveauPlateau)),
            writeln("Vous pouvez entrer 'stop' à tout moment pour quitter."),
            jouer_tour(ProchainJoueur, TypeJoueur1, TypeJoueur2)
        )
    ;
        writeln("Colonne invalide ou pleine, réessayez."),
        jouer_tour(Joueur, TypeJoueur1, TypeJoueur2)
    ).