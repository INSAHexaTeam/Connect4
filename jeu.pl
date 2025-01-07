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
        % Stocker le plateau dans une référence globale
        nb_setval(plateau_courant, Plateau),
        nb_setval(joueur_courant, 'X'),
        % Modifier le message pour passer les bons paramètres
        new(B, button(Col, message(@prolog, jouer_colonne))),
        send(B, attribute, colonne, Col),
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

% Modifier le prédicat jouer_colonne pour utiliser les variables globales
jouer_colonne(Colonne) :-
    nb_getval(plateau_courant, Plateau),
    nb_getval(joueur_courant, Joueur),
    (ajouter_pion(Plateau, Colonne, Joueur, NouveauPlateau) ->
        nb_setval(plateau_courant, NouveauPlateau),
        % Afficher le plateau dans le terminal
        nl, write('Joueur '), write(Joueur), write(' joue colonne '), write(Colonne), nl,
        afficher_plateau(NouveauPlateau),
        nl,
        (verifier_victoire(NouveauPlateau, Joueur) ->
            afficher_victoire(Joueur)
        ;
            changer_joueur(Joueur, ProchainJoueur),
            nb_setval(joueur_courant, ProchainJoueur),
            mettre_a_jour_boutons(NouveauPlateau, ProchainJoueur)
        )
    ;
        send(@display, inform, 'Colonne invalide ou pleine, reessayez.')
    ).

% Mettre à jour les boutons
mettre_a_jour_boutons(Plateau, Joueur) :-
    get(@event?receiver?device?device, member, picture, BoutonsPanel),
    send(BoutonsPanel, clear),
    % Recréer les boutons
    forall(between(1, 7, Col), (
        new(B, button(Col, message(@prolog, jouer_colonne))),
        send(B, attribute, colonne, Col),
        send(B, size, size(20, 20)),
        X is (Col-1) * 60 + 10,
        send(BoutonsPanel, display, B, point(X, 5))
    )),
    % Afficher le tour du joueur
    format(atom(Message), 'Tour du joueur ~w', [Joueur]),
    send(@display, inform, Message).

% Ajouter un prédicat pour obtenir la colonne depuis le bouton
jouer_colonne :-
    get(@event?receiver, attribute, colonne, Colonne),
    jouer_colonne(Colonne).

% Mettre à jour l'affichage graphique
mettre_a_jour_affichage(Plateau) :-
    % Afficher dans le terminal
    nl,  % Nouvelle ligne pour plus de clarté
    write('État actuel du plateau :'), nl,
    % Compléter les colonnes avec des variables pour les cases vides
    completer_colonnes(Plateau, 6, PlateauComplete),
    afficher_plateau(PlateauComplete),
    nl.

% Compléter chaque colonne avec des variables jusqu'à la hauteur désirée
completer_colonnes([], _, []).
completer_colonnes([Colonne|Reste], Hauteur, [ColonneComplete|ResteComplete]) :-
    length(Colonne, Taille),
    NbVars is Hauteur - Taille,
    length(Vars, NbVars),
    append(Colonne, Vars, ColonneComplete),
    completer_colonnes(Reste, Hauteur, ResteComplete).

% Afficher le message de victoire
afficher_victoire(Joueur) :-
    format(atom(Message), 'Le joueur ~w a gagné !', [Joueur]),
    send(@display, inform, Message).
