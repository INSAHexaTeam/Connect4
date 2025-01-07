:- use_module(library(pce)).

% Prédicat principal pour démarrer l'interface
demarrer_interface :-
    new(D, dialog('Menu - Morpion')),
    send(D, gap, size(20, 20)),
    send(D, size, size(400, 300)),
    
    % Titre
    new(Titre, label(titre, 'MORPION')),
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
    send(B, size, size(200, 40)),  % Taille fixe pour tous les boutons
    send(Dialog, append, B).

% Action pour quitter
quitter_jeu :-
    send(@display, confirm, 'Voulez-vous vraiment quitter ?'),
    send(@display?frame, destroy).

% Afficher les règles du jeu
afficher_regles :-
    new(D, dialog('Regles du Morpion')),
    send(D, size, size(300, 200)),
    
    new(Texte, text('Regles du jeu :')),
    send(Texte, font, font(helvetica, bold, 14)),
    send(D, append, Texte),
    send(D, append, label(space, '')),
    
    new(Regles, text),
    send(Regles, font, font(helvetica, normal, 12)),
    send(Regles, append, '- Les joueurs jouent tour a tour\n'),
    send(Regles, append, '- Le premier a aligner 3 symboles gagne\n'),
    send(Regles, append, '- X commence toujours en premier'),
    send(D, append, Regles),
    
    new(Bouton, button('OK', message(D, destroy))),
    send(Bouton, font, font(helvetica, normal, 12)),
    send(D, append, Bouton),
    
    send(D, open_centered).

% Placeholder pour nouvelle partie
nouvelle_partie :-
    new(D, dialog('Information')),
    send(D, size, size(300, 100)),
    
    new(Message, text('Cette fonctionnalite sera\ndisponible prochainement')),
    send(Message, font, font(helvetica, normal, 12)),
    send(D, append, Message),
    
    new(Bouton, button('OK', message(D, destroy))),
    send(Bouton, font, font(helvetica, normal, 12)),
    send(D, append, Bouton),
    
    send(D, open_centered).

% Démarrage du menu au chargement
:- initialization(demarrer_interface).
