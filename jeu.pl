% Chargement des modules nécessaires
:- consult('gestion/affichage.pl').  % Module pour l'affichage du plateau
:- consult('gestion/victoire.pl').   % Module pour vérifier les conditions de victoire
:- use_module('ia/aleatoire').       % Module pour l'IA aléatoire
:- use_module('ia/minimax', [choisir_colonne_minimax/2]).         % Module pour l'IA Minimax
:- use_module('gestion/joueurs', [joueur_peut_jouer/1, changer_joueur/2]).


% Déclaration pour manipuler dynamiquement le plateau
:- dynamic plateau_actuel/1.

% Plateau initial vide
plateau_vide([[], [], [], [], [], [], []]).

% Débuter le jeu
jouer :-
    retractall(plateau_actuel(_)),  % Supprime tout plateau existant
    plateau_vide(Plateau),
    assert(plateau_actuel(Plateau)),  % Initialise le plateau
    choisir_mode_jeu.

% Choix du mode de jeu
choisir_mode_jeu :-
    writeln("Choisissez le mode de jeu :"),
    writeln("1. Joueur vs Joueur"),
    writeln("2. Joueur vs IA (aléatoire)"),
    writeln("3. Joueur vs IA (Minimax)"),
    writeln("4. Quitter"),
    read(Mode),
    ( Mode = 1 -> jouer_tour('X', humain, humain)
    ; Mode = 2 -> jouer_tour('X', humain, ia_aleatoire)
    ; Mode = 3 -> jouer_tour('X', humain, ia_minimax)
    ; Mode = 4 -> writeln("Au revoir !"), halt
    ; writeln("Mode invalide, veuillez réessayer."), choisir_mode_jeu ).

% Gérer un tour de jeu
jouer_tour(Joueur, TypeJoueur1, TypeJoueur2) :-
    plateau_actuel(Plateau),  % Récupère le plateau actuel
    afficher_plateau(Plateau),
    ( Joueur = 'X' -> TypeJoueur = TypeJoueur1 ; TypeJoueur = TypeJoueur2 ),
    demander_colonne(Joueur, Colonne, TypeJoueur),
    ( joueur_peut_jouer(Plateau, Colonne) ->
        simuler_coup(Plateau, Colonne, Joueur, NouveauPlateau),
        ( verifier_victoire(NouveauPlateau, Joueur) ->
            afficher_plateau(NouveauPlateau),
            format("Le joueur ~w a gagné !\n", [Joueur]),
            halt
        ; changer_joueur(Joueur, ProchainJoueur),
          retractall(plateau_actuel(_)),
          assert(plateau_actuel(NouveauPlateau)),
          jouer_tour(ProchainJoueur, TypeJoueur1, TypeJoueur2)
        )
    ; writeln("Colonne invalide ou pleine, réessayez."),
      jouer_tour(Joueur, TypeJoueur1, TypeJoueur2) ).

% Demander la colonne à jouer selon le type de joueur
demander_colonne(Joueur, Colonne, humain) :-
    format("Joueur ~w, choisissez une colonne (0-6) : ", [Joueur]),
    read(Colonne).

demander_colonne(Joueur, Colonne, ia_aleatoire) :-
    writeln("L IA aléatoire joue..."),
    ia_aleatoire:choisir_colonne(Joueur, Colonne).

demander_colonne(Joueur, Colonne, ia_minimax) :-
    writeln("L IA Minimax réfléchit..."),
    % Appel à votre implémentation de choisir_colonne_minimax
    minimax:choisir_colonne_minimax(Joueur, Colonne).

% Vérifier si un joueur peut jouer dans une colonne
joueur_peut_jouer(Plateau, Colonne) :-
    nth0(Colonne, Plateau, Liste),
    length(Liste, Longueur),
    Longueur < 6.  % Une colonne ne peut pas dépasser 6 jetons

% Simuler un coup (ajouter un jeton dans une colonne)
simuler_coup(Plateau, Colonne, Joueur, NouveauPlateau) :-
    nth0(Colonne, Plateau, ColonneListe, AutresColonnes),
    append(ColonneListe, [Joueur], NouvelleColonne),
    nth0(Colonne, NouveauPlateau, NouvelleColonne, AutresColonnes).

