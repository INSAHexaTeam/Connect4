:- use_module(library(pce)).
:- [jeu].

% Prédicat principal pour démarrer l'interface
demarrer_interface :-
    % [jeu].  % Sans extension .pl
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
    jouer.  % Appel direct du prédicat jouer du fichier jeu.pl

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
    format('Colonne selectionnee: ~w~n', [Col]).

% Action pour quitter
quitter_jeu :-
    (   send(@display, confirm, 'Voulez-vous vraiment quitter ?')
    ->  send(@display, reset),  % Nettoie toutes les fenêtres
        retractall(grille(_)),  % Nettoie les données du jeu
        halt                    % Quitte Prolog
    ;   true                   % Ne rien faire si l'utilisateur annule
    ).

% Afficher les règles du jeu
afficher_regles :-
    new(D, dialog('Regles du Puissance 4')),
    send(D, size, size(300, 200)),
    
    new(Texte, text('Regles du jeu :')),
    send(Texte, font, font(helvetica, bold, 14)),
    send(D, append, Texte),
    send(D, append, label(space, '')),
    
    new(Regles, text),
    send(Regles, font, font(helvetica, normal, 12)),
    send(Regles, append, '- Les joueurs jouent tour a tour\n'),
    send(Regles, append, '- Le premier a aligner 4 jetons gagne\n'),
    send(Regles, append, '- Les jetons tombent en bas de la colonne'),
    send(D, append, Regles),
    
    new(Bouton, button('OK', message(D, destroy))),
    send(Bouton, font, font(helvetica, normal, 12)),
    send(D, append, Bouton),
    
    send(D, open_centered).

% Démarrage du menu au chargement
:- initialization(demarrer_interface).
