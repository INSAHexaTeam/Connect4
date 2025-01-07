:- use_module(library(pce)).
:- encoding(utf8).
:- consult('jeu.pl').  % Charger le fichier jeu.pl

% État de la dernière partie (pour l'affichage décoratif)
:- dynamic derniere_partie/1.
% Compteurs de victoires
:- dynamic score/2.  % score(Couleur, NombreVictoires)

% Initialiser les scores
:- assertz(score(rouge, 0)),
   assertz(score(jaune, 0)).

% Initialiser l'état de la dernière partie (grille vide)
:- plateau_vide(PlateauVide),
   assertz(derniere_partie(PlateauVide)).

% Prédicat principal pour démarrer l'interface
demarrer_interface :-
    new(D, dialog('Menu - Puissance 4')),
    send(D, name, menu),  % Donner un nom à la fenêtre pour la retrouver plus tard
    send(D, recogniser, click_gesture(left, '', single, 
        message(@prolog, afficher_coordonnees, @event?position))),
    send(D, gap, size(80, 40)),
    send(D, size, size(400, 400)),


    % Espace
    send(D, append, label(space, '')),
    
    % Création de la grille décorative
    new(Grille, picture),
    send(Grille, size, size(195, 165)),
    send(D, append, Grille, below), % Position relative au dernier élément
    
    % Dessin du fond bleu
    new(Fond, box(210, 180)),
    send(Fond, fill_pattern, colour(blue)),
    send(Grille, display, Fond),
    
    % Création des cercles et affichage de la dernière partie
    derniere_partie(Plateau),
    forall(between(0, 5, Ligne),
           forall(between(0, 6, Col),
                  creer_cercle_menu(Grille, Plateau, Ligne, Col))),
    
    % Affichage des scores
    
    % Création d'un conteneur vertical pour les boutons
    new(VBox, dialog_group('')),

    new(Titre, label(titre, 'PUISSANCE 4')),
    send(Titre, font, bold),
    send(VBox, append, Titre),

    send(VBox, size, size(400, 400)),
    send(VBox, gap, size(10, 20)),  % Espacement entre les boutons

    
    
    % % Score Jaune
    score(jaune, ScoreJaune),
    atomic_list_concat([ScoreJaune,' :'], TexteJaune),
    new(JauneLabel, text(TexteJaune)),
    send(JauneLabel, colour, black),
    send(JauneLabel, font, font(helvetica, bold, 12)),
    send(VBox, display, JauneLabel,point(205,301)),

    % Score Rouge
    score(rouge, ScoreRouge),
    atomic_list_concat([': ', ScoreRouge], TexteRouge),
    new(RougeLabel, text(TexteRouge)),
    send(RougeLabel, colour, black),
    send(RougeLabel, font, font(helvetica, bold, 12)),
    send(VBox, display, RougeLabel,point(150,301)),

    % Text VS
    new(VSLabel, text('VS')),
    send(VSLabel, colour, black),
    send(VSLabel, font, font(helvetica, bold, 12)),
    send(VBox, display, VSLabel,point(178,301)),

    send(D, append, VBox),

    %Cercle jaune
    new(Cercle, circle(12)),
    send(Cercle, center, point(230, 310)),
    send(Cercle, fill_pattern, colour(yellow)),
    send(D, display, Cercle),

    %Cercle rouge
    new(Cercle2, circle(12)),
    send(Cercle2, center, point(140, 310)),
    send(Cercle2, fill_pattern, colour(red)),
    send(D, display, Cercle2),

    




    % Boutons du menu
    creer_bouton(VBox, 'JOUER', nouvelle_partie,point(40,350)),
    creer_bouton(VBox, 'REGLES DU JEU', afficher_regles,point(130,350)),
    creer_bouton(VBox, 'QUITTER', quitter_jeu,point(280,350)),
    
    send(D, open_centered).

% Création d'un cercle dans la grille du menu
creer_cercle_menu(Grille, Plateau, Ligne, Col) :-
    X is Col * 30 + 15,
    Y is Ligne * 30 + 15,
    get_pion(Plateau, Ligne, Col, Pion),
    couleur_pion(Pion, Couleur),
    new(Cercle, circle(12)),
    send(Cercle, center, point(X, Y)),
    send(Cercle, fill_pattern, colour(Couleur)),
    send(Grille, display, Cercle).

% Création d'un bouton standardisé
creer_bouton(Dialog, Texte, Action,Pos) :-
    new(B, button(Texte, message(@prolog, Action))),
    send(B, font, font(helvetica, normal, 14)),
    send(B, size, size(200, 40)),
    send(Dialog, display, B,Pos).

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
    send(D, append, BoutonsPanel),
    
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
    % Sauvegarder l'état final de la partie
    etat_jeu(Plateau, _),
    retractall(derniere_partie(_)),
    assertz(derniere_partie(Plateau)),
    % Mettre à jour le score
    (Joueur = 'X' -> Couleur = rouge ; Couleur = jaune),
    score(Couleur, Score),
    NouveauScore is Score + 1,
    retract(score(Couleur, Score)),
    assertz(score(Couleur, NouveauScore)),
    % Fermer la fenêtre de jeu
    nb_getval(frame_ref, Frame),
    send(Frame, destroy),
    % Nettoyer les références
    nb_delete(frame_ref),
    nb_delete(grille_ref),
    % Mettre à jour la grille du menu
    mettre_a_jour_menu.

% Mettre à jour la grille du menu
mettre_a_jour_menu :-
    % Trouver la fenêtre du menu
    get(@display, frame, menu, D),
    % Trouver la grille dans la fenêtre
    get(D, member, picture, Grille),
    % Effacer le contenu actuel
    send(Grille, clear),
    % Redessiner le fond bleu
    new(Fond, box(210, 180)),
    send(Fond, fill_pattern, colour(blue)),
    send(Grille, display, Fond),
    % Redessiner les cercles avec le nouvel état
    derniere_partie(Plateau),
    forall(between(0, 5, Ligne),
           forall(between(0, 6, Col),
                  creer_cercle_menu(Grille, Plateau, Ligne, Col))),
    % Mettre à jour les scores
    get(D, member, dialog_group, ScoreBox),
    get(ScoreBox, member, text, RougeLabel),
    get(ScoreBox, member, text, JauneLabel),
    score(rouge, ScoreRouge),
    score(jaune, ScoreJaune),
    atomic_list_concat(['Rouge : ', ScoreRouge], TexteRouge),
    atomic_list_concat(['Jaune : ', ScoreJaune], TexteJaune),
    send(RougeLabel, string, TexteRouge),
    send(JauneLabel, string, TexteJaune).

% Démarrage du menu au chargement
:- initialization(demarrer_interface).

% Prédicat pour afficher les coordonnées d'un clic
afficher_coordonnees(Position) :-
    get(Position, x, X),
    get(Position, y, Y),
    format('Clic aux coordonnées: (~w, ~w)~n', [X, Y]).
