% Fichier principal : jeu.pl
:- consult('gestion/affichage.pl').
:- consult('gestion/joueurs.pl').
:- consult('gestion/victoire.pl').
:- use_module('ia/aleatoire').
:- use_module('ia/minimax', [simuler_coup/4]).  % Import explicite de simuler_coup

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
    writeln("4. Quitter"),
    read(Mode),
    (Mode = 1 ->
        jouer_tour('X', humain, humain)
    ; Mode = 2 ->
        jouer_tour('X', humain, ia_aleatoire)
    ; Mode = 3 ->
        jouer_tour('X', humain, ia_minimax)
    ; Mode = 4 ->
        writeln("Au revoir !"), halt
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
