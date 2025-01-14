% Fichier principal : jeu.pl
:- consult('gestion/affichage.pl').
:- consult('gestion/joueurs.pl').
:- consult('gestion/victoire.pl').
:- consult('gestion/ia.pl').
:- use_module(library(pce)).

% État du jeu
:- dynamic etat_jeu/2.  % etat_jeu(Plateau, JoueurCourant)

% Plateau initial
plateau_vide([[], [], [], [], [], [], []]).

% Initialiser une nouvelle partie
initialiser_partie :-
    plateau_vide(Plateau),
    retractall(etat_jeu(_, _)),
    assertz(etat_jeu(Plateau, 'X')).

% Démarrer le jeu (version interface graphique)
demarrer_jeu :-
    initialiser_partie.

% Jouer un coup depuis l'interface
jouer_coup_interface(Colonne) :-
    etat_jeu(Plateau, Joueur),
    (Joueur = 'X' ->
        % Tour du joueur humain (rouge)
        (ajouter_pion(Plateau, Colonne, Joueur, NouveauPlateau) ->
            retract(etat_jeu(Plateau, Joueur)),
            assertz(etat_jeu(NouveauPlateau, 'O')),
            mettre_a_jour_interface(NouveauPlateau),
            mettre_a_jour_indicateur_tour('O'),
            (verifier_victoire(NouveauPlateau, Joueur) ->
                annoncer_victoire(Joueur)
            ; 
                % Forcer l'exécution du tour de l'IA immédiatement
                call(jouer_tour_ia)
            )
        ; 
            send(@display, inform, 'Coup invalide !')
        )
    ; true).

% Tour de l'IA modifié pour être plus robuste
jouer_tour_ia :-
    etat_jeu(Plateau, 'O'),
    % Ajouter un petit délai pour que le coup de l'IA soit visible
    sleep(0.5),
    % S'assurer que l'IA trouve un coup valide
    (jouer_coup_ia(Plateau, NouveauPlateau) ->
        retract(etat_jeu(Plateau, 'O')),
        assertz(etat_jeu(NouveauPlateau, 'X')),
        mettre_a_jour_interface(NouveauPlateau),
        mettre_a_jour_indicateur_tour('X'),
        (verifier_victoire(NouveauPlateau, 'O') ->
            annoncer_victoire('O')
        ; true)
    ;
        % En cas d'échec de l'IA, afficher un message d'erreur
        send(@display, inform, 'Erreur: L\'IA n\'a pas pu jouer !')
    ).
