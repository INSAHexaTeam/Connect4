% Fichier : ia/aleatoire.pl

:- module(aleatoire, [choisir_colonne_ia/1]).

% IA : choisir une colonne aléatoire valide
choisir_colonne_ia(Colonne) :-
    repeat,
    random(1, 8, Colonne),  % Génère une colonne aléatoire entre 1 et 7
    joueur_peut_jouer(Colonne),
    !.

% Vérifie si une colonne est jouable
joueur_peut_jouer(Colonne) :-
    plateau_actuel(Plateau),  % Récupère le plateau actuel
    nth1(Colonne, Plateau, ListeColonne),
    length(ListeColonne, Taille),
    Taille < 6.  % La colonne n'est pas pleine
