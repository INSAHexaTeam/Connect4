:- use_module(library(pce)).
:- encoding(utf8).
:- consult('jeu.pl').  % Charger le fichier jeu.pl

% Prédicat principal pour démarrer l'interface
demarrer_interface :-
    new(D, dialog('Menu - Puissance 4')),
    send(D, gap, size(20, 20)),
    send(D, size, size(400, 300)),
    
    % Titre
    new(Titre, label(titre, 'PUISSANCE 4')),
    send(Titre, font, bold),
    send(D, append, Titre),
    
    % Espace
    send(D, append, label(space, '')),
    
    % Boutons du menu
    creer_bouton(D, 'JOUER', nouvelle_partie),
    creer_bouton(D, 'REGLES DU JEU', afficher_regles),
    creer_bouton(D, 'QUITTER', quitter_jeu),
    
    send(D, open_centered).

% Création d'un bouton standardisé
creer_bouton(Dialog, Texte, Action) :-
    new(B, button(Texte, message(@prolog, Action))),
    send(B, font, font(helvetica, normal, 14)),
    send(B, size, size(200, 40)),
    send(Dialog, append, B).

% Création de la grille de jeu
nouvelle_partie :-
    new(F, frame('Puissance 4')),
    nb_setval(frame_ref, F),  % Stocker la référence à la frame
    send(F, size, size(800, 650)),
    
    new(D, dialog),
    send(F, append, D),
    
    % Création des boutons pour chaque colonne dans un dialog horizontal
    new(BoutonsPanel, picture),
    send(BoutonsPanel, size, size(440, 50)),
    % send(BoutonsPanel, gap, size(0, 0)),
    % send(BoutonsPanel, background, colour(yellow)),
    send(D, append, BoutonsPanel),
    
    % Création d'un panel pour contenir les boutons
    % new(ButtonBox, box(440, 50)),
    % send(ButtonBox, fill_pattern, colour(yellow)),
    % send(BoutonsPanel, display, ButtonBox),
    
    % Création des boutons avec positionnement absolu
    forall(between(1, 7, Col), (
        new(B, button(Col, message(@prolog, jouer_colonne, Col))),
        send(B, size, size(20, 20)),          % Boutons plus grands
        X is (Col-1) * 60 + 10,              % Ajustement de l'espacement
        send(BoutonsPanel, display  , B, point(X, 5))
    )),
    
    % Création de la grille visuelle (6x7)
    new(Grille, picture),
    send(Grille, size, size(420, 360)),
    send(D, append, Grille, below),
    
    % Dessin du fond bleu
    new(Fond, box(420, 360)),
    send(Fond, fill_pattern, colour(blue)),
    send(Grille, display, Fond),
    
    % Création des cercles
    forall(between(0, 5, Ligne),
           forall(between(0, 6, Col),
                  creer_cercle(Grille, Ligne, Col))),
    
    demarrer_jeu,  % Initialise une nouvelle partie
    
    % Stocke la référence à la grille pour les mises à jour
    nb_setval(grille_ref, Grille),
    
    send(F, open_centered).

% Création d'un cercle dans la grille
creer_cercle(Grille, Ligne, Col) :-
    X is Col * 60 + 30,
    Y is Ligne * 60 + 30,
    new(Cercle, circle(25)),  % Diamètre de 50 pixels
    send(Cercle, center, point(X, Y)),
    send(Cercle, fill_pattern, colour(white)),
    send(Grille, display, Cercle).

% Gestion du clic sur une colonne
jouer_colonne(Col) :-
    jouer_coup_interface(Col).

% Action pour quitter
quitter_jeu :-
    (   send(@display, confirm, 'Voulez-vous vraiment quitter ?', @default, @default, 'utf8')
    ->  send(@display, reset),  % Nettoie toutes les fenêtres
        retractall(grille(_)),  % Nettoie les données du jeu
        halt                    % Quitte Prolog
    ;   true                   % Ne rien faire si l'utilisateur annule
    ).

% Afficher les règles du jeu
afficher_regles :-
    new(D, dialog('Règles du Puissance 4')),
    send(D, size, size(300, 200)),
    
    new(Texte, text('Règles du jeu :')),
    send(Texte, font, font(helvetica, bold, 14)),
    send(D, append, Texte),
    send(D, append, label(space, '')),
    
    new(Regles, text),
    send(Regles, font, font(helvetica, normal, 12)),
    send(Regles, append, '- Les joueurs jouent tour à tour\n'),
    send(Regles, append, '- Le premier à aligner 4 jetons gagne\n'),
    send(Regles, append, '- Les jetons tombent en bas de la colonne'),
    send(D, append, Regles),
    
    new(Bouton, button('OK', message(D, destroy))),
    send(Bouton, font, font(helvetica, normal, 12)),
    send(D, append, Bouton),
    
    send(D, open_centered).

% Prédicat pour mettre à jour l'interface graphique
mettre_a_jour_interface(Plateau) :-
    nb_getval(grille_ref, Grille),
    forall(between(0, 5, Ligne),
           forall(between(0, 6, Col),
                  mettre_a_jour_case(Grille, Plateau, Ligne, Col))).

% Mettre à jour une case spécifique
mettre_a_jour_case(Grille, Plateau, Ligne, Col) :-
    X is Col * 60 + 30,
    Y is Ligne * 60 + 30,
    get_pion(Plateau, Ligne, Col, Pion),
    couleur_pion(Pion, Couleur),
    send(Grille, display, new(C, circle(25))),
    send(C, center, point(X, Y)),
    send(C, fill_pattern, colour(Couleur)).

% Définir la couleur selon le pion
couleur_pion('X', red).
couleur_pion('O', yellow).
couleur_pion(_, white).

% Obtenir le pion à une position donnée
get_pion(Plateau, Ligne, Col, Pion) :-
    nth0(Col, Plateau, Colonne),
    Pos is 5 - Ligne,  % Inverser la ligne car l'affichage est de haut en bas
    (nth0(Pos, Colonne, Pion) -> true ; Pion = ' ').

% Annoncer la victoire
annoncer_victoire(Joueur) :-
    (Joueur = 'X' -> CouleurJoueur = 'Rouge' ; CouleurJoueur = 'Jaune'),
    atomic_list_concat(['Le joueur ', CouleurJoueur, ' a gagné !'], Message),
    send(@display, inform, Message, @default, @default, 'utf8'),
    % Fermer la fenêtre de jeu
    nb_getval(frame_ref, Frame),
    send(Frame, destroy),
    % Nettoyer les références
    nb_delete(frame_ref),
    nb_delete(grille_ref).

% Démarrage du menu au chargement
:- initialization(demarrer_interface).