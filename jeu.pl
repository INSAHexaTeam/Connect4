% Fichier principal : jeu.pl
:- consult('gestion/affichage.pl').
:- consult('gestion/joueurs.pl').
:- consult('gestion/victoire.pl').

% Déclaration pour manipuler dynamiquement le plateau
:- dynamic plateau_actuel/1.

% Plateau initial vide
plateau_vide([[], [], [], [], [], [], []]).

% Démarrer le jeu
jouer :-
    retractall(plateau_actuel(_)),  % Supprimer tout plateau existant
    plateau_vide(Plateau),
    assert(plateau_actuel(Plateau)),  % Initialiser le plateau
    choisir_mode_jeu.

% Choisir le mode de jeu
choisir_mode_jeu :-
    writeln("Choisissez le mode de jeu :"),
    writeln("1. Joueur vs Joueur"),
    writeln("2. Joueur vs IA"),
    read(Mode),
    (Mode = 1 ->
        jouer_tour('X', 'humain', 'humain')
    ; Mode = 2 ->
        jouer_tour('X', 'humain', 'ia')
    ;
        writeln("Mode invalide, recommencez."),
        choisir_mode_jeu
    ).

% Gérer un tour de jeu
jouer_tour(Joueur, TypeJoueur1, TypeJoueur2) :-
    plateau_actuel(Plateau),  % Récupérer le plateau actuel
    afficher_plateau(Plateau),
    (Joueur = 'X' -> TypeJoueur = TypeJoueur1; TypeJoueur = TypeJoueur2),
    demander_colonne(Joueur, Colonne, TypeJoueur),
    (ajouter_pion(Plateau, Colonne, Joueur, NouveauPlateau) ->
        (verifier_victoire(NouveauPlateau, Joueur) ->
            retractall(plateau_actuel(_)),
            assert(plateau_actuel(NouveauPlateau)),
            afficher_plateau(NouveauPlateau),
            format("Le joueur ~w a gagné !", [Joueur])
        ;
            changer_joueur(Joueur, ProchainJoueur),
            retractall(plateau_actuel(_)),
            assert(plateau_actuel(NouveauPlateau)),
            jouer_tour(ProchainJoueur, TypeJoueur1, TypeJoueur2)
        )
    ;
        writeln("Colonne invalide ou pleine, réessayez."),
        jouer_tour(Joueur, TypeJoueur1, TypeJoueur2)
    ).


