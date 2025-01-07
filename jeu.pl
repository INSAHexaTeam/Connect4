% Fichier principal : jeu.pl
:- consult('gestion/affichage.pl').
:- consult('gestion/joueurs.pl').
:- consult('gestion/victoire.pl').
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
    (ajouter_pion(Plateau, Colonne, Joueur, NouveauPlateau) ->
        retract(etat_jeu(Plateau, Joueur)),
        changer_joueur(Joueur, ProchainJoueur),
        assertz(etat_jeu(NouveauPlateau, ProchainJoueur)),
        mettre_a_jour_interface(NouveauPlateau),
        (verifier_victoire(NouveauPlateau, Joueur) ->
            annoncer_victoire(Joueur)
        ; true)
    ; 
        send(@display, inform, 'Coup invalide !')
    ).
