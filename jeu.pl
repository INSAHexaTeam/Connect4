% Fichier principal : jeu.pl
:- consult('gestion/affichage.pl').
:- consult('gestion/joueurs.pl').
:- consult('gestion/victoire.pl').

% Plateau initial
plateau_vide([[], [], [], [], [], [], []]).

% Démarrer le jeu
jouer :-
    plateau_vide(Plateau),
    jouer_tour(Plateau, 'X').

% Gérer un tour de jeu
jouer_tour(Plateau, Joueur) :-
    afficher_plateau(Plateau),
    demander_colonne(Joueur, Colonne),
    (ajouter_pion(Plateau, Colonne, Joueur, NouveauPlateau) ->
        (verifier_victoire(NouveauPlateau, Joueur) ->
            afficher_plateau(NouveauPlateau),
            format("Le joueur ~w a gagné !", [Joueur])
        ;
            changer_joueur(Joueur, ProchainJoueur),
            jouer_tour(NouveauPlateau, ProchainJoueur)
        )
    ;
        writeln("Colonne invalide ou pleine, réessayez."),
        jouer_tour(Plateau, Joueur)
    ).
