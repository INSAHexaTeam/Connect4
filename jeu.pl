% Fichier principal : jeu.pl
:- consult(gestion/affichage),
   consult(gestion/joueurs),
   consult(gestion/victoire).

% Plateau initial
plateau_vide([[], [], [], [], [], [], []]).

% Démarrer le jeu
jouer :-
    plateau_vide(Plateau),
    creer_fenetre_jeu(Plateau),
    jouer_tour(Plateau, 'X').

% Créer la fenêtre de jeu
creer_fenetre_jeu(Plateau) :-
    new(F, frame('Puissance 4')),
    send(F, size, size(800, 650)),
    
    new(D, dialog),
    send(F, append, D),
    
    % Création des boutons pour chaque colonne
    new(BoutonsPanel, picture),
    send(BoutonsPanel, size, size(440, 50)),
    send(D, append, BoutonsPanel),
    
    % Création des boutons avec positionnement absolu
    forall(between(1, 7, Col), (
        new(B, button(Col, message(@prolog, jouer_colonne, Plateau, Col, 'X'))),
        send(B, size, size(20, 20)),
        X is (Col-1) * 60 + 10,
        send(BoutonsPanel, display, B, point(X, 5))
    )),
    
    % Création de la grille visuelle
    new(Grille, picture),
    send(Grille, size, size(420, 360)),
    send(D, append, Grille, below),
    
    % Dessin du fond
    new(Fond, box(420, 360)),
    send(Fond, fill_pattern, colour(blue)),
    send(Grille, display, Fond),
    
    % Création des cercles
    forall(between(0, 5, Ligne),
           forall(between(0, 6, Col),
                  creer_cercle(Grille, Ligne, Col))),
    
    send(F, open_centered).

% Gérer un tour de jeu (version graphique)
jouer_tour(Plateau, Joueur) :-
    % La logique du tour sera gérée par les événements des boutons
    true.

% Gestion du clic sur une colonne
jouer_colonne(Plateau, Colonne, Joueur) :-
    (ajouter_pion(Plateau, Colonne, Joueur, NouveauPlateau) ->
        mettre_a_jour_affichage(NouveauPlateau),
        (verifier_victoire(NouveauPlateau, Joueur) ->
            afficher_victoire(Joueur)
        ;
            changer_joueur(Joueur, ProchainJoueur),
            mettre_a_jour_boutons(NouveauPlateau, ProchainJoueur)
        )
    ;
        send(@display, inform, 'Colonne invalide ou pleine, reessayez.')
    ).

% Mettre à jour l'affichage graphique
mettre_a_jour_affichage(Plateau) :-
    % À implémenter : mise à jour visuelle du plateau
    true.

% Afficher le message de victoire
afficher_victoire(Joueur) :-
    format(atom(Message), 'Le joueur ~w a gagné !', [Joueur]),
    send(@display, inform, Message).
